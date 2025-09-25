/' -- Lossy Image Compression with Randomized Circles - 2025 Sep 25  by dafhi

    Initial rollout by Gemini.
    
    one cool benefit from gemini's version
      error caculation is a direct measurement of
    accum buf, circle brightness, and source image
  
    before, i wrote circle to accum buf, error calc'd,
    then erased to prep next randomization
  
      update:
    evolutionary grid, lots of minor adjustments
    
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


    Namespace custom_rng
  Const As ULongInt mulA = &b0000000001000000000100000000100000001000000100000100001000100101
  Const As ULongInt mulB = &b1101001100001000000000010000000000000000100100000000000010000001
  Dim As ULongInt a
Function v(seed As ULongInt) As ULongInt
    a = mulA * (a Xor seed) + 1
    Return a
End Function
End Namespace

  function f_rng( seed as ulongint = 0 ) as double
      return custom_rng.v(seed) / ( 2^64 + (2048 + 2) )
  end function

    #define rng  f_rng

#define min( a,b) iif( (a)<(b), (a), (b) )
#define max( a,b) iif( (a)>(b), (a), (b) )

function clamp( in As double, hi As double = 1, lo As double = 0) As double
  return min( max(in, lo), hi )
End Function

function round(in as double, places as ubyte = 3) as string ' 2024 Aug 4
    dim as integer mul = 10 ^ places
    dim as string  ret = str( int(in * mul + .5) / mul )
    return left( ret, places + instr( ret, ".") )
End Function

function sqr_safe( d as double ) as double
    return sgn(d) * sqr( abs(d))
end function


    type ACCUM_TYPE as short

Type CircleShape
    as single       x, y, radius
    As ACCUM_TYPE   brightness ' Can be positive or negative
    as short        seed ' handled by group header
End Type

type t_hyper_parameter
    declare constructor( as single, as single = 1, as single = 0, as string = "" )
    declare operator cast as single
    declare operator let( as single )
    declare sub mutate( as single = 1, as single = -1 )
    as single hi = 1
    as single lo = 0
    as single f, _inc
    as string name
end type

constructor t_hyper_parameter( _f as single, _hi as single, _lo as single, nam as string )
    f = _f : hi = _hi : lo = _lo : name = nam
end constructor
operator t_hyper_parameter.cast as single : return f
end operator
operator t_hyper_parameter.let( _f as single )
    f = _f
end operator
sub t_hyper_parameter.mutate( hi as single, lo as single )
    
    dim as single a = lo + rnd * (hi - lo), b = hi + rnd * (lo - hi) '' experimental
    
    f = clamp( a+rnd*(b-a), this.hi, this.lo ) '' linear interpolation with clamp
end sub

Const BLUE_IDX  = 0 ' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
Const GREEN_IDX = 1
Const RED_IDX   = 2
Const ALPHA_IDX = 3


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

    dim as ubyte    component_average(0 to 2), current_channel_idx ' Which channel (R,G,B)
    dim as double   g_radius, g_total_energy
    dim as long     pos_neg, g_cbits_total

Sub setup_channel_processing( imva as imvars, source_image As Any Ptr, channel_idx As Integer)
    if imva.im <> source_image then fill_imvars(imva, source_image)
      
        dim as long wa = ubound(accumulator_buffer,1) + 1
        dim as long ha = ubound(accumulator_buffer,2) + 1
    if imva.w<>wa or imva.h<>ha then redim accumulator_buffer(imva.w-1,imva.h-1)
    
    g_total_energy = 0  '' 2025 Sep 24
    For y As Long = 0 To imva.h - 1
        For x As Long = 0 To imva.w - 1
            g_total_energy += accumulator_buffer(x, y) * accumulator_buffer(x, y)
        Next
    Next
    g_radius = (imva.w + imva.h)
    current_channel_idx = channel_idx
End Sub

Sub render_circle_to_accum_buf( c As CircleShape, reconstruction_phase as long = false )
    
    _cliprect c, imv
    pos_neg = iif(reconstruction_phase, -c.brightness, c.brightness)
    Dim As single r_squared = c.radius * c.radius

        For y As Long = y0 To y1
    Dim As single dy = y - c.y
    dim as single r     = sqr_safe(r_squared - dy*dy)' - .5 ' + .5 fixed an anti-aliasing project
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
    dim as single r     = sqr_safe(r_squared - dy*dy)' - .5 ' + .5 fixed an anti-aliasing project
    
        For x As Long = max( c.x - r, x0 ) To min( c.x + r, x1 ) step _step
    dim as long er0 = accumulator_buffer(x,y) ' Gemini described this as residual processing .. no source image involved
    dim as long er1 = accumulator_buffer(x,y) + c.brightness
    dim as long improvement = er0*er0 - er1*er1
    score += improvement
    
    csamps += 1 ' sparse sampling variable
        Next x
    Next y
    
    Return score * csamps
End Function


type hyper_parameters
    as t_hyper_parameter cbits_base = type( 2.5, 2, 0, "bits base" ) '' initial, hi, lo
    as t_hyper_parameter cbits_mag = type( 5, 8, 0, "bits mag" )
    as t_hyper_parameter cbits_decay = type( .996, .9999, .91, "bits dcy" )
    as t_hyper_parameter rad_mult_a = type( 1.33, 1.45, 1.05, "rad mul a" )
    as t_hyper_parameter br_decay = type( .997, .9999, .6, "br dcy" )
    as t_hyper_parameter br_var_decay = type( .997, .9999, .6, "br var dcy" )
    as t_hyper_parameter br_mag = type( 45, 60, .5, "br mag" )
    as t_hyper_parameter br_var_mag = type( 40, 140, 10, "br var mag" )
    as t_hyper_parameter radius_clamp_div = type( 7, 23, 4, "rcd" )
    as single score
end type

  dim as hyper_parameters hypers

  function f_cbits( shape_index as long ) as long
      return hypers.cbits_base + hypers.cbits_mag * hypers.cbits_decay ^ (shape_index - 1)
  end function

  function f_radius as double
        dim as single f = rng, hard_rad_min = 0.77, hard_rad_max = (imv.w + imv.h) / hypers.radius_clamp_div
      dim as single a = hypers.rad_mult_a, b = 2 - a
      return clamp( g_radius * ( a*f + b*(1-f) ), hard_rad_max, hard_rad_min )
  end function
    
  function f_brightness_base_factor( shape_index as long ) as single
      return hypers.br_decay ^ (shape_index - 1)
  end function

  function f_brightness_variance_factor( shape_index as long ) as single
      return hypers.br_var_decay ^ (shape_index - 1)
  end function
    dim as single   br_base, br_var, xy_a, xy_b, bright, rad

  sub internal_vars_from_rng( shape_index as long)
      xy_a = rng
      xy_b = rng
      rad = f_radius
        br_base = f_brightness_base_factor( shape_index )
        br_var  = 1 - f_brightness_variance_factor( shape_index )
        pos_neg = iif( rng < .5, -1, 1 )
      bright = pos_neg * ( 45 * br_base + rng * ( 1 + 40 * br_var ) )
  end sub

  sub shape_from_rng( c as circleShape, shape_index as long )
    internal_vars_from_rng shape_index
    c.radius = rad
    c.brightness = bright
    c.x = imv.w * xy_a
    c.y = imv.h * xy_b
  end sub

    static As CircleShape best_fit, candidate
    dim as ushort         g_random_seed, cbits_base, cbits

    sub shape_props_from_seeds( c as CircleShape, shape_index as long, seed as long )
          using custom_rng
        a = current_channel_idx xor g_random_seed
        a xor= a shl 1
        a xor= seed * mulB
        a xor= a shr 1
        a xor= shape_index * mulA
        shape_from_rng c, shape_index
        c.seed = seed
    end sub

Function find_best_shape( shape_index as long ) As CircleShape
    Dim As double best_score = -1e12
    cbits = cbits_base + f_cbits(shape_index)
    dim as long iterations = 2 ^ cbits
        For seed As long = 1 To iterations
      shape_props_from_seeds candidate, shape_index, seed
    Dim As double score = calculate_fitness(candidate)
    If score > best_score Then
        best_score = score
        best_fit = candidate
    End If
    Next
    g_cbits_total += cbits
    Return best_fit
End Function

End Namespace


        Namespace orchestrator
    
    dim as ushort seeds()    
    Using hashed_shapes_single_channel

Sub encode( source_image as any ptr, channel_idx As Integer, num_circles As Integer )

    setup_channel_processing( imv, source_image, channel_idx)
    if num_circles > ubound(seeds) then redim seeds(num_circles)
    
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
        best_fit = find_best_shape(i)
        seeds(i) = best_fit.seed
        render_circle_to_accum_buf best_fit
        g_radius = best_fit.radius
    Next i

End Sub

Sub decode( target_image As Any Ptr, channel_idx As Integer )
        static as imvars imvars_t
    setup_channel_processing( imvars_t, target_image, channel_idx)

    For y As Long = 0 To imvars_t.h - 1
    For x As Long = 0 To imvars_t.w - 1
        accumulator_buffer(x,y) = component_average(channel_idx)
    Next : next

        For i As Integer = 1 to ubound(seeds)
    dim as long reconstruction = true
      
      shape_props_from_seeds best_fit, i, seeds(i)'circle_list(i).seed
    
    render_circle_to_accum_buf best_fit, reconstruction
    g_radius = best_fit.radius
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

sub entire_one_channel( src as any ptr, des as any ptr, _
                        chan_idx as long, num_circles as long )
    static As CircleShape circles()
    
    cbits_base = 0'log(search_iterations) / log(2) + .49999  '' c++ .99999
    
    encode(src, chan_IDX, num_circles )
    decode des, chan_IDX
end sub

End Namespace

Function compute_psnr(original As Any Ptr, reconstructed As Any Ptr) As Double '' Qwen
    Dim As imvars orig, recon
    fill_imvars orig, original
    fill_imvars recon, reconstructed

    Dim As Double mse = 0, pixel_count = orig.w * orig.h * 3
    Dim As UByte Ptr o_px = orig.pixels, r_px = recon.pixels

    For y As Long = 0 To orig.h - 1
        Dim As UByte Ptr o_row = o_px + y * orig.pitch
        Dim As UByte Ptr r_row = r_px + y * recon.pitch
        For x As Long = 0 To orig.w - 1
            For c As Long = 0 To 2  '' RGB
                Dim As Long o_val = o_row[x * 4 + c]
                Dim As Long r_val = r_row[x * 4 + c]
                Dim As Long diff = o_val - r_val
                mse += diff * diff
            Next
        Next
    Next

    If mse = 0 Then Return 100
    mse /= pixel_count
    Return 10 * Log(65025 / mse) / Log(10)  '' 255^2 = 65025
End Function

' Logarithmic Efficiency (penalizes large bit increases)
function log_efficiency_score(psnr as double, bits as long) as double
    if bits <= 0 then return 0
    return psnr / log(1 + bits)  ' PSNR / log(Rate+1)
end function

    using orchestrator

    sub print_param( t as t_hyper_parameter, x as long = 0, y as long = 0 )
        draw string (x,y), t.name + " " + round(t.f, 3)
    end sub

sub test( src as any ptr, final_image as any ptr, ix as long = 0, iy as long = 0, num_shapes as long = 100 )

      g_cbits_total = 0
    
    entire_one_channel src, final_image, RED_IDX, num_shapes
    entire_one_channel src, final_image, Green_IDX, num_shapes
    entire_one_channel src, final_image, Blue_IDX, num_shapes
    ix*=imv.w : iy*=imv.h
    Put (ix, iy), final_image, PSet
    ix += 10' : iy += 5
    print_param hypers.br_mag, ix, iy + 10
    print_param hypers.br_var_mag, ix, iy + 20
    hypers.score = log_efficiency_score( compute_psnr( src, final_image ), g_cbits_total )
    draw string (ix, iy + 45), "log efficiency(PSNR, bits) " + round( 1 * hypers.score, 2)
    draw string (ix + 180, iy + 65), "bytes " + str((g_cbits_total + 7)shr 3)
  
end sub

dim shared as long best_ix, best_iy

sub plot_grid( src as any ptr, des as any ptr, shapes_per_chan as long, hy() as hyper_parameters, selected_ix as long, selected_iy as long )
    if selected_ix < 0 or selected_iy < 0 or selected_ix > 1 or selected_iy > 1 then exit sub
    dim as hyper_parameters sel = hy(selected_ix, selected_iy)
    
        for y as long = 0 to 1
    for x as long = 0 to 1
    hypers = sel
    hypers.cbits_base.mutate 3, 1.5
    hypers.cbits_mag.mutate 7, 1
    hypers.cbits_decay.mutate .9999, .95
    hypers.br_mag.mutate 60, 1
    hypers.br_var_mag.mutate 150, 1
    test src, des, x, y, shapes_per_chan
    if hypers.score > hy(x,y).score then
    hy(x,y) = hypers
    endif
    if hypers.score > hy(best_ix, best_iy).score then
    best_ix = x : best_iy = y
    endif
    next
    next
    hypers = hy(best_ix,best_iy)
    test src, des, best_ix, best_iy, shapes_per_chan
    
end sub

sub testpattern( imv as imvars )
    dim as long seed = 40, w = imv.w, h = imv.h, diagonal = sqr(w*w + h*h)
    randomize seed
    for i as long = 1 to 10
        dim as ulong col = rnd*culng(-1)
        if rnd < .25 then
            line imv.im, (rnd*w,rnd*h)-(rnd*w,rnd*h), col
        elseif rnd < .30 then
            line imv.im, (rnd*w,rnd*h)-(rnd*w,rnd*h), col, bf
        else
            circle imv.im, (rnd*w, rnd*h), diagonal * (.01 + rnd * .15), col,,,,f
        endif
    next
end sub

' =============================================================================
'   MAIN PROGRAM
' =============================================================================

Const IMG_W = 356
Const IMG_H = 256

dim as long shapes_per_chan = 100

ScreenRes IMG_W * 2 + 30, IMG_H * 2 + 20, 32

const black = rgb(0,0,0)

Dim As Any Ptr source_image = ImageCreate(IMG_W, IMG_H, 0, Black)

dim as imvars imv : fill_imvars imv, source_image
testpattern imv
Put (10, 10), source_image, PSet
Draw String (20, 15), "Original Image"
sleep 1000

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)

  #define extchar chr(255) '' L Shift, Alt, etc.
dim as string kstr = ""
dim as single help_time = 4

dim as double t = timer, dt, tp, t0 = t

' Mouse support variables
dim as integer mouse_x, mouse_y, mouse_btn, mouse_btn_prev, hy_index
dim as hyper_parameters hypers_clicked, hy(2,2), hypers_original

  '' obtain an initial score and fill grid params
'test source_image, final_image, 50, 50, shapes_per_chan
for y as long = 0 to 2 : for x as long = 0 to 2
hy(x,y) = hypers
: next : next

dim as long new_generation = true

randomize

do
    tp = t : t = timer
    dt = t - tp
    
    getmouse mouse_x, mouse_y, , mouse_btn
    
    if mouse_btn = 1 and mouse_btn_prev = 0 then
        new_generation = true
        best_ix = (mouse_x - 0) \ IMG_W
        best_iy = (mouse_y - 0) \ IMG_H
    end if
    mouse_btn_prev = mouse_btn
    
    select case (kstr)
    case ""
    case "a", "A"
        new_generation = true
    case extchar + chr(72) '' up
        if shapes_per_chan < 800 then
        shapes_per_chan += max(1, .2*shapes_per_chan)
        new_generation = true
        endif
    case extchar + chr(80) '' down
        if shapes_per_chan > 1 then
        shapes_per_chan -= max(1, shapes_per_chan* 0.18 )
        new_generation = true
        endif
    case "h"
        help_time = t + 10 - t0
    case chr(27) '' esc
        exit do
    end select
    
      if new_generation then
    for i as long = 1 to 1
    plot_grid source_image,final_image, shapes_per_chan, hy(), best_ix, best_iy
    next
    new_generation = false
    endif
    
    if t - t0 < help_time then
    locate 11,1
    print "  Keys: " + chr(10) + _
    "                  " + chr(10) + _
    "  h          : help" + chr(10) + _
    "                  " + chr(10) + _
    "  a          : copy best, mutate" + chr(10) + _
    "  up down    : circle count" + chr(10) + _
    "                  " + chr(10) + _
    " Mouse: copy tile, mutate"
    endif
    
    sleep 1
    kstr = inkey
loop

ImageDestroy(source_image)
ImageDestroy(final_image)
