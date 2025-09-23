/' -- Lossy Image Compression with Randomized Circles - 2025 Sep 23  by dafhi

    Initial rollout by Gemini.
    
    one cool benefit from gemini's version
      error caculation is a direct measurement of
    accum buf, circle brightness, and source image
  
    before, i wrote circle to accum buf, error calc'd,
    then erased to prep next randomization
  
      update:
    keyboard -> adjust bit depth
    
'/


Type imvars
    As Long      w, h, bypp, pitch
    As Any Ptr   pixels, im
End Type

      ' -- image class helpers
  sub _gfx_release( byref im as any ptr )
    if im <> 0 then if imageinfo(im) = 0 then imagedestroy im
    im = 0
  end sub

Sub fill_imvars( Byref i As imvars, im As Any Ptr = 0)
    _gfx_release i.im
    If im = 0 Then
        ScreenInfo i.w, i.h, , i.bypp, i.pitch: i.pixels = screenptr
    Else
        ImageInfo im, i.w, i.h, i.bypp, i.pitch, i.pixels: i.im = im
    End If
End Sub


    namespace custom_rng '' 2025 Sep 21

const as ulongint     mulA  = &b0000000001000000000100000000100000001000000100000100001000100101
const as ulongint     mulB  = &b1101001100001000000000010000000000000000100100000000000010000001 ' 2025 Sep 12
const as ulongint     xorC  = &b0101010101010101010101010101010101010101010101010101010101010101

dim as ulongint       a, b, c,d,e, ro, xorshifted

function v( seed as ulongint ) as ulongint
    a = mulA * (a xor seed) + 1
    return a
end function

end namespace ' -- _rng

  function f_rng( seed as ulongint = 0 ) as double
      return custom_rng.v(seed) / ( 2^64 + (2048 + 2) )
  end function

  #define rng  f_rng

#define min( a,b) iif( (a)<(b), (a), (b) )
#define max( a,b) iif( (a)>(b), (a), (b) )

function clamp( in As double, hi As double = 1, lo As double = 0) As double
  return min( max(in, lo), hi )
End Function

function sqr_safe( d as double ) as double
    return sgn(d) * sqr( abs(d))
end function


' A structure to hold the properties of a single circle.
Type CircleShape
    as single   radius, x, y
    As long     brightness ' Can be positive or negative

    as short    seed
End Type

sub print_shape_props( c as circleshape )
    print "x y rad a : "; c.x; " "; c.y; " "; c.radius; " "; c.brightness; "  "; c.seed
end sub
' -----------

Const BLUE_IDX  = 0 ' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
Const GREEN_IDX = 1
Const RED_IDX   = 2
Const ALPHA_IDX = 3

#define ACCUM_TYPE Short

    Namespace hashed_shapes_single_channel

dim as long x0, x1, y0, y1, ix0, ix1

sub _cliprect( c as CircleShape, imv as imvars )
    x0 = max( c.x - c.radius, 0 )
    y0 = max( c.y - c.radius, 0 )
    x1 = min( c.x + c.radius, imv.w - 1 )
    y1 = min( c.y + c.radius, imv.h - 1 )
end sub

Dim As imvars     imv, imvars_t ' Holds info about the source/dest image
Dim As ACCUM_TYPE accumulator_buffer(any, any) ' The 2-byte signed buffer

sub show( channel_idx as long )
      if imvars_t.w <> imv.w or imvars_t.h <> imv.h then _
    fill_imvars imvars_t, imagecreate(imv.w,imv.h, rgb(0,0,0))

    line imvars_t.im, (0,0)-(imv.w-1, imv.h-1),rgb(0,0,0), bf
    Dim As UByte Ptr dest_pixels = Cptr(UByte Ptr, imvars_t.pixels)
        
        For y As Long = 0 To imvars_t.h - 1
    Dim As UByte Ptr row_ptr = dest_pixels + y * imvars_t.pitch
        For x As Long = 0 To imvars_t.w - 1
    row_ptr[x * 4 + channel_idx] = clamp( accumulator_buffer(x,y), 255)
    Next
    Next
    
    put (0,0), imvars_t.im, pset
end sub

    dim as ubyte    component_average(0 to 2), current_channel_idx ' Which channel (R,G,B)
    dim as double   g_radius
    dim as long     pos_neg, g_cbits_total

Sub setup_channel_processing( imva as imvars, source_image As Any Ptr, channel_idx As Integer)
    if imva.im <> source_image then fill_imvars(imva, source_image)
      
        dim as long wa = ubound(accumulator_buffer,1) + 1
        dim as long ha = ubound(accumulator_buffer,2) + 1
    if imva.w<>wa or imva.h<>ha then redim accumulator_buffer(imva.w-1,imva.h-1)
    
    g_radius = (imva.w + imva.h)
    current_channel_idx = channel_idx
End Sub

Sub render_circle_to_accum_buf( c As CircleShape, reconstruction_phase as long = false )
    
    _cliprect c, imv
    pos_neg = iif(reconstruction_phase, -c.brightness, c.brightness)
    Dim As single r_squared = c.radius * c.radius

        For y As Long = y0 To y1
    Dim As single dy = y - c.y
    dim as single r     = sqr_safe(r_squared - dy*dy) - .5 '  .. i forget why 
        For x As Long = max( c.x - r, x0 ) To min( c.x + r, x1 )
    accumulator_buffer(x,y) += pos_neg
    Next x
    Next y
  
End Sub

Function calculate_fitness(c As CircleShape) As double
    Dim As longint score = 0
    
    _cliprect c, imv
    
    Dim As Long r_squared = c.radius * c.radius
  
    '' sparse sampling
    dim as long csamps, _step = max( sqr(c.radius * .4), 1 )
        
        For y As Long = y0 To y1 step _step
    Dim As single dy = y - c.y
    
    ' circle-constrained scan width
    dim as single r     = sqr_safe(r_squared - dy*dy) - .5 '  .. i forget why 
    
        For x As Long = max( c.x - r, x0 ) To min( c.x + r, x1 ) step _step
    dim as long er0 = accumulator_buffer(x,y) ' Gemini described this as residual .. no source image involved
    dim as long er1 = accumulator_buffer(x,y) + c.brightness
    dim as long improvement = er0*er0 - er1*er1
    score += improvement
    
    csamps += 1 ' sparse sampling variable
        Next x
    Next y
    
    Return score * csamps
End Function

  function f_radius as double
        dim as single f = rng, hard_rad_min = 0.77, hard_rad_max = (imv.w + imv.h) / 14
      return clamp( g_radius * ( 1.20*f + 0.68*(1-f) ), hard_rad_max, hard_rad_min )
  end function
    
  function f_brightness_base_factor( shape_index as long ) as single
      return .998 ^ (shape_index - 1)
  end function

  function f_brightness_variance_factor( shape_index as long ) as single
      return .998 ^ (shape_index - 1)
  end function
  
    dim as single   f_br_base, f_br_var, xy_a, xy_b, bright, rad

  sub internal_vars_from_rng( shape_index as long)
      xy_a = rng
      xy_b = rng
      rad = f_radius
        f_br_base = f_brightness_base_factor( shape_index )
        f_br_var = f_brightness_variance_factor( shape_index )
        pos_neg = iif( rng < .5, -1, 1 )
      bright = pos_neg * ( .5 + 40 * f_br_base + rng * ( 1.5 + 30 * f_br_var) )
  end sub

  sub shape_from_rng( c as circleShape, shape_index as long )
    internal_vars_from_rng shape_index
    c.radius = rad
    c.brightness = bright
    c.x = imv.w * xy_a
    c.y = imv.h * xy_b
  end sub

    static As CircleShape best_fit, candidate, residual
    dim as ushort         g_random_seed

    sub shape_props_from_seed( byref c as CircleShape, shape_index as long, seed as long )
          using custom_rng
        a = current_channel_idx xor g_random_seed
        a xor= a shl 1
        a xor= seed * mulB
        a xor= a shr 1
        a xor= shape_index * mulA
        shape_from_rng c, shape_index
        c.seed = seed
    end sub

Function find_best_group(iterations As Integer, shape_index as long, xor_seed as long = 0 ) As CircleShape
    Dim As double best_score = -1e12

        For seed As long = 1 To iterations
      shape_props_from_seed candidate, shape_index, seed xor xor_seed
    Dim As double score = calculate_fitness(candidate)
    If score > best_score Then
        best_score = score
        best_fit = candidate
    End If
    Next
    Return best_fit
End Function

End Namespace


    Namespace hsf
    
  Using hashed_shapes_single_channel

Sub process_channel( source_image as any ptr, channel_idx As Integer, circle_list() As CircleShape, _
    num_circles As Integer, search_iterations As Integer)

    setup_channel_processing( imv, source_image, channel_idx)
    if num_circles <> ubound(circle_list) then Redim circle_list(1 To num_circles)
    
      ' --- average channel brightness
    Dim As UByte Ptr src_pix = Cptr(UByte Ptr, imv.pixels)
    Dim As Double total_brightness = 0
    For y As Long = 0 To imv.h - 1
        Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
        For x As Long = 0 To imv.w - 1
            total_brightness += row_ptr[x * 4 + channel_idx]
        Next
    Next
    component_average(channel_idx) = total_brightness / (imv.w*imv.h)
    
    ' --- copy source channel and subtract the average
        For y As Long = 0 To imv.h-1
    Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
        For x As Long = 0 To imv.w-1
    accumulator_buffer(x,y) = _
    row_ptr[x * 4 + channel_idx] - component_average(channel_idx)
    Next: next
    
    For i As long = 1 To num_circles
        best_fit = find_best_group(search_iterations, i)
        circle_list(i) = best_fit
        render_circle_to_accum_buf best_fit
        g_radius = best_fit.radius
    Next i
    
End Sub

Sub construct_target_channel( circle_list() As CircleShape, target_image As Any Ptr, channel_idx As Integer )
        static as imvars imvars_t
    setup_channel_processing( imvars_t, target_image, channel_idx)

    For y As Long = 0 To imvars_t.h - 1
    For x As Long = 0 To imvars_t.w - 1
        accumulator_buffer(x,y) = component_average(channel_idx)
    Next : next

        For i As Integer = LBound(circle_list) To UBound(circle_list)
    dim as long reconstruction = true
      
      shape_props_from_seed circle_list(i), i, circle_list(i).seed
    
    render_circle_to_accum_buf circle_list(i), reconstruction
    g_radius = circle_list(i).radius
    Next i
    
    ' copy to corresponding target channel
    Dim As UByte Ptr dest_pixels = Cptr(UByte Ptr, imvars_t.pixels)
    For y As Long = 0 To imvars_t.h - 1
        Dim As UByte Ptr row_ptr = dest_pixels + y * imvars_t.pitch
        For x As Long = 0 To imvars_t.w - 1
            row_ptr[x * 4 + channel_idx] = clamp( accumulator_buffer(x,y), 255 )
        Next
    Next

End Sub

  dim as long debug_var

sub entire_one_channel( src as any ptr, des as any ptr, _
                        chan_idx as long, num_circles as long, search_iterations as long )
      static As CircleShape circles()
    
    hsf.process_channel(src, chan_IDX, circles(), num_circles, SEARCH_ITERATIONS)
    hsf.construct_target_channel circles(), des, chan_IDX
    if debug_var then hsf.show chan_idx: sleep
end sub

End Namespace

sub test( source_image as any ptr, final_image as any ptr, num_circles as long, _
          c_iters as long, x as long = 0, y as long = 0, _debug_var as long = 0 )
      
        using hsf
      
      debug_var = _debug_var
      g_cbits_total = 0
    
    g_random_seed = rnd * cushort(-1)
    
    entire_one_channel source_image, final_image, RED_IDX, num_circles, c_iters
    entire_one_channel source_image, final_image, Green_IDX, num_circles, c_iters
    entire_one_channel source_image, final_image, Blue_IDX, num_circles, c_iters
    
    Put (x, y), final_image, PSet
        
    dim as long bits_per_group = log(c_iters) / log(2)
    g_cbits_total = 3 * bits_per_group * num_circles
    
    Draw String (x + 20, y + 5), str(num_circles ) + " circles per channel  " _
    + str(8 + (g_cbits_total + 7)shr 3) + " bytes .. (8 byte header)"
    Draw String (x + 20, y + 15), "bit depth " + str(bits_per_group)
end sub


' =============================================================================
'   MAIN PROGRAM
' =============================================================================

Const IMG_W = 456
Const IMG_H = 456

dim as long bit_depth = 8
dim as long circles_per_channel = 43

ScreenRes IMG_W * 2 + 30, IMG_H + 20, 32

const black = rgb(0,0,0)

' --- 1. Create a procedural test image ---
Dim As Any Ptr source_image = ImageCreate(IMG_W, IMG_H)', 0, Black)
For y As Integer = 0 To IMG_H - 1
    For x As Integer = 0 To IMG_W - 1
        Dim As UByte r = x
        Dim As UByte g = y
        Dim As UByte b = 255 - (x/2 + y/2)
        PSet source_image, (x, y), RGBA(r, g, b, 255)
    Next
Next
Put (10, 10), source_image, PSet
Draw String (20, 15), "Original Image"

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)', 0, Black)
      
      randomize
    using hsf

dim as long SEARCH_ITERATIONS   = 2^bit_depth

  #define extchar chr(255) '' L Shift, Alt, etc.
dim as string kstr = "a"
dim as single help_time = 4

dim as double t = timer, dt, tp, t0 = t

do
    tp = t : t = timer
    dt = t - tp
      dim as long new_generation
    
    select case (kstr)
    case ""
    case chr(27) '' esc
        exit do
    case "a", "A"
        new_generation = true
    case extchar + chr(75) '' left
        bit_depth = max( 1, bit_depth - 1)
        SEARCH_ITERATIONS   = 2^bit_depth
        new_generation = true
    case extchar + chr(77) '' right
        if bit_depth < 12 then
        bit_depth += 1
        SEARCH_ITERATIONS   = 2^bit_depth
        new_generation = true
        endif
    case extchar + chr(72) '' up
        if circles_per_channel < 800 then
        circles_per_channel *= 1.2
        new_generation = true
        endif
    case extchar + chr(80) '' down
        if circles_per_channel > 20 then
        circles_per_channel *= 0.8
        new_generation = true
        endif
    case "h"
        help_time = t + 10 - t0
    end select
    
      if new_generation then _
    test source_image, final_image, circles_per_channel, SEARCH_ITERATIONS, IMG_W + 20, 10
    
    if t - t0 < help_time then
    locate 4,1
    print "  Keys: " + chr(10) + _
    "                  " + chr(10) + _
    " h          : help" + chr(10) + _
    "                  " + chr(10) + _
    " a          : new generation" + chr(10) + _
    " left right : bit depth" + chr(10) + _
    " up down    : circle count"
    endif
    
    sleep 1
    kstr = inkey
loop

ImageDestroy(source_image)
ImageDestroy(final_image)
