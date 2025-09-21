/' -- hashed shapes image compression (proof of concept) - 2025 Sep 21  by dafhi

    Initial rollout by Gemini from my ability to recount previous work.
  
    Bonus: Gemini's version didn't create a backup buffer, but instead
    projected the shape's result
  
    Also bonus: i found the inspiration to "deep think" my old idea of
    eliminating source image from fitness calculation.
  
    Gemini referred to it as residual error,
    and Qwen also introduced the term energy-based processing.
  
      basics:
    copy source image to accumulation buffer, subtract avg brightness.
      (buffer will have positive and negative regions)
    circle fitness is a measure of accum buffer movement towards zero.
      (gotta remember to er0*er0 - er1*er1 (or use Abs) )
   
      
      update:
    removed bit masks from individual properties, adjusting
    instead from property order in randomization
  
      near-term goal:
    automate hyperparameter search

  
      about:
    inspired by genetic algorithm triangles video around 2014.
    I figured i could do it at least 1000x quicker, and with circles.

    if you're interested in another simple algorithms with excellent
    compression potential
  
    string art - https://www.freebasic.net/forum/viewtopic.php?t=33107

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

  #define min( a,b) iif( (a)<(b), (a), (b) )
  #define max( a,b) iif( (a)>(b), (a), (b) )

function clamp( in As double, hi As double = 1, lo As double = 0) As double
  return min( max(in, lo), hi )
End Function


' A structure to hold the properties of a single circle.
Type CircleShape
    as single x, y
    as single radius
    As single brightness ' Can be positive or negative
  
    as ulongint seed, rng_a
End Type

sub print_shape_props( c as circleshape )
    print "x y rad a : "; c.x; " "; c.y; " "; c.radius; " "; c.brightness
end sub


    namespace my_rng
const as ulongint     mulA  = &b0000000001000000000100000000100000001000000100000100001000100101
const as ulongint     mulB  = &b1101001100001000000000010000000000000000100100000000000010000001
const as ulongint     xorC  = &b0101010101010101010101010101010101010101010101010101010101010101
  dim as ulongint       a, b, c,d,e, ro, xorshifted
function v( seed as ulongint ) as ulongint
    a = mulB * (a xor seed) + mulA
    return a
end function
end namespace ' -- my_rng


  function rng( seed as ulongint = 0 ) as double
      return my_rng.v(seed) / ( 2^64 + (2048 + 2) )
  end function


' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
Const BLUE_IDX  = 0
Const GREEN_IDX = 1
Const RED_IDX   = 2
Const ALPHA_IDX = 3

#define ACCUM_TYPE Short

    Namespace ns__single_channel_processing

dim as long x0, x1, y0, y1

sub _cliprect( c as CircleShape, imva as imvars )
    x0 = max( c.x - c.radius, 0 )
    y0 = max( c.y - c.radius, 0 )
    x1 = min(c.x + c.radius, imva.w - 1 )
    y1 = min(c.y + c.radius, imva.h - 1 )
end sub

' -- Shared variables within the namespace --
Dim As imvars     imv ' Holds info about the source/dest image
Dim As ACCUM_TYPE accumulator_buffer(any, any) ' The 2-byte signed buffer
dim as ubyte      component_average(0 to 2), current_channel_idx ' Which channel (R,G,B) we're working on
  
    dim as double g_radius

dim as single hyper_param(9) '' will probably automate a parameter search
dim as single cbits_initial
dim as single cbits_decay_factor

  ' initializes processing for one channel.  saves some memory
Sub setup_channel_processing( imva as imvars, source_image As Any Ptr, channel_idx As Integer, circle_list() as circleshape)', num_circles as long )
        
    if imva.im <> source_image then
    fill_imvars(imva, source_image)
    endif
    
      dim as long w = ubound(accumulator_buffer,1) + 1
      dim as long h = ubound(accumulator_buffer,2) + 1
    if imva.w<>w or imva.h<>h then
        redim accumulator_buffer(imva.w-1,imva.h-1)
        'Redim edge_map(imv.w-1, imv.h-1)
    endif
    'compute_sobel channel_idx
    
    current_channel_idx = channel_idx
    
      hyper_param(0) = 2.0 '' will automate search for good hyper-param value
    g_radius = (imva.w + imva.h) / hyper_param(0)
End Sub

  function f_adaptive_radius( seed as long = 0) as double
  
        dim as long f = rng(seed) ' dim as double  or  as long
      
      hyper_param(1) = 1.08
      hyper_param(2) = 0.91
      
      hyper_param(3) = 2.75
        
        return clamp( _
      g_radius * ( hyper_param(1) * f + hyper_param(2) * (1-f) ), _
      max(imv.w,imv.h) / hyper_param(3), _
      .85 )
      
  end function


Function calculate_fitness(c As CircleShape) As double
    Dim As longint score = 0
    
    _cliprect c, imv
    Dim As Long r_squared = c.radius * c.radius, csamps
  
    '' sparse sampling
    dim as long _step = iif( c.radius < 6, 1, sqr(c.radius * .4) )
        
        For y As Long = y0 To y1 step _step
    Dim As Long dy = y - c.y, dySq = dy * dy
        
        For x As Long = x0 + rnd*_step To x1 step _step
    Dim As Long dx = x - c.x
        If (dx*dx + dySq) > r_squared Then continue for
    
    ' Gemini described this as residual processing (no source image involved)
    dim as long er0 = accumulator_buffer(x,y)
    dim as long er1 = accumulator_buffer(x,y) + c.brightness
    dim as long improvement = er0*er0 - er1*er1
    
    score += improvement' * (0.5 + edge_map(x,y))  '' baseline 0.5, boost on edges
    
    csamps += 1 ' sparse sampling variable
        
        Next x
    Next y
    Return score * csamps
End Function

  sub states_from_seed( byref c as CircleShape, shape_index as long, seed as long )
        using my_rng
        
      a = xorC shr current_channel_idx
      a xor= shape_index * mulA
      
      a += (seed * mulA)shr 1 '' Nov 21 - seed reintroduced
      
      c.rng_a = a
      c.seed = seed
  end sub
  
  function f_brightness_base( shape_index as long ) as single
      return .9987 ^ (shape_index - 1)
  end function
  
  function f_brightness_variance( shape_index as long ) as single
      return .9987 ^ (shape_index - 1)
  end function
  
 
  function f_cbits( shape_index as long ) as long
      
        '' some of these may become hyper params
        cbits_initial = 7.5
        cbits_decay_factor = 0.97
      return 2. + cbits_initial * cbits_decay_factor ^ (shape_index - 1)
  
  end function

  dim as long   pos_neg, g_cbits_total
  dim as single br_base, br_var, xy_a, xy_b
  
  sub props_common( c as circleshape, shape_index as long )
      
      /'  quality depends on sequence (x y rad brightness)
      
        a b rad bright ****
        a rad bright b  ****
        a bright b rad  ***
      
      '/
      
          xy_a = rng
          xy_b = rng
        if imv.h > imv.w then swap xy_a, xy_b '' swap if image height is the major axis
      c.x = imv.w * xy_a
      c.y = imv.h * xy_b

      c.radius = f_adaptive_radius
      
          pos_neg = iif( rng < .5, -1, 1 )
          br_base = f_brightness_base( shape_index )
          br_var = f_brightness_variance( shape_index )
        hyper_param(4) = 30
        hyper_param(5) = 20
      c.brightness = (1 + hyper_param(4) * br_base + rng * (1 + hyper_param(5) * br_var) ) * pos_neg
  
  end sub
  
  sub props_from_states( c as circleshape, seed as long, shape_index as long )
          
      my_rng.a = c.rng_a ' set the rng state
      
      props_common c, shape_index
      
  end sub


Function find_best_circle( shape_index as long ) As CircleShape
    static As CircleShape best_circle, candidate
    Dim As double best_score = -1e12
    dim as long cbits = f_cbits(shape_index)

        for seed as long = 1 to 2^cbits
    states_from_seed candidate, shape_index, seed
    props_from_states candidate, seed, shape_index
    Dim As double score = calculate_fitness(candidate)
    If score > best_score Then
        best_score = score
        best_circle = candidate
    End If
    Next
    g_cbits_total += cbits
    
    Return best_circle
End Function

Sub render_circle_to_accum_buf( byval c As CircleShape, shape_index as long, reconstruction_phase as long = false )
    
    props_from_states c, c.seed, shape_index
    _cliprect c, imv
    
    Dim As Long r_squared = c.radius * c.radius
    if reconstruction_phase then c.brightness = -c.brightness
    
        For y As Long = y0 To y1
    Dim As Long dy = y - c.y, dySq = dy * dy
        
        For x As Long = x0 To x1
    Dim As Long dx = x - c.x
    If (dx*dx + dySq) > r_squared Then continue for
    
    Dim As Long new_val = accumulator_buffer(x,y) + c.brightness
    accumulator_buffer(x,y) = clamp(new_val, 32767, -32768)
    
    Next x
    Next y
  
    g_radius = c.radius

End Sub

End Namespace ' -- ns__single_channel_processing


    Namespace hsf ' -- hashed shapes format
    
Using ns__single_channel_processing

sub show( channel_idx as long )
    static as imvars imvars_t
    if imvars_t.w <> imv.w or imvars_t.h <> imv.h then
    fill_imvars imvars_t, imagecreate(imv.w,imv.h, rgb(0,0,0))
    endif
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

Sub process_channel( source_image as any ptr, channel_idx As Integer, circle_list() As CircleShape, num_circles As Integer)

    Redim circle_list(1 To num_circles)
    setup_channel_processing imv, source_image, channel_idx, circle_list()
    
      ' --- calculate channel average brightness ---
    Dim As UByte Ptr src_pix = Cptr(UByte Ptr, imv.pixels)
    Dim As Double total_brightness = 0
    For y As Long = 0 To imv.h - 1
        Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
        For x As Long = 0 To imv.w - 1
            total_brightness += row_ptr[x * 4 + channel_idx]
        Next
    Next
    component_average(channel_idx) = total_brightness / (imv.w*imv.h)
    
    ' --- copy image channel, subtract the average
        For y As Long = 0 To imv.h-1
    Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
        For x As Long = 0 To imv.w-1
    accumulator_buffer(x,y) = _
    row_ptr[x * 4 + channel_idx] - component_average(channel_idx)
    Next: next
    ' -------------------------------------------------------------------------
    
'    Print "Processing channel " & channel_idx & "..."
    For i As Integer = 1 To num_circles
'        If (i Mod 100 = 0) Then Print "  Circle " & i & " of " & num_circles
        Dim As CircleShape best_fit = find_best_circle(i)
        circle_list(i) = best_fit
'        print_shape_props best_fit
        render_circle_to_accum_buf best_fit, i
    Next

'    Print "Channel " & channel_idx & " processing complete."
End Sub

Sub construct_target_channel( circle_list() As CircleShape, target_image As Any Ptr, channel_idx As Integer )
    static as imvars imvars_t
    setup_channel_processing imvars_t, target_image, channel_idx, circle_list()

    For y As Long = 0 To imvars_t.h - 1
    For x As Long = 0 To imvars_t.w - 1
        accumulator_buffer(x,y) = component_average(channel_idx)
    Next : next
    ' ------------------------------------------------

        For i As Integer = LBound(circle_list) To UBound(circle_list)
    dim as long reconstruction = true
    render_circle_to_accum_buf circle_list(i), i, reconstruction
    Next

    ' copy to corresponding target channel
    Dim As UByte Ptr dest_pixels = Cptr(UByte Ptr, imvars_t.pixels)
    For y As Long = 0 To imvars_t.h - 1
        Dim As UByte Ptr row_ptr = dest_pixels + y * imvars_t.pitch
        For x As Long = 0 To imvars_t.w - 1
            row_ptr[x * 4 + channel_idx] = clamp( accumulator_buffer(x,y), 255 )
        Next
    Next
End Sub

sub entire_one_channel( src as any ptr, des as any ptr, chan_idx as long, num_circles as long = 40 )
      static As CircleShape circles()
    hsf.process_channel src, chan_IDX, circles(), num_circles
    hsf.construct_target_channel circles(), des, chan_IDX
'    hsf.show chan_idx: sleep '300
end sub

End Namespace ' -- hsf


' =============================================================================
'   MAIN PROGRAM
' =============================================================================

sub test( source_image as any ptr, final_image as any ptr, num_circles as long, x as long = 0, y as long = 0 )
      
      hsf.g_cbits_total = 0
    
    hsf.entire_one_channel source_image, final_image, RED_IDX, num_circles
    hsf.entire_one_channel source_image, final_image, Green_IDX, num_circles
    hsf.entire_one_channel source_image, final_image, Blue_IDX, num_circles
    
    Put (x, y), final_image, PSet
        
    Draw String (x + 20, y + 5), " size : " + str(6 + (hsf.g_cbits_total + 7)shr 3) + " bytes .. (6 byte header)"

end sub


Const IMG_W = 556
Const IMG_H = 556

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
Put (20, 0), source_image, PSet
Draw String (30, 5), "Original Image"
sleep 200

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)', 0, Black)

dim as long CIRCLES_PER_CHANNEL = 15

' --- 2. "Compress" each channel by finding the best circles ---
test source_image, final_image, circles_per_channel, 20, 0
test source_image, final_image,                 250, IMG_W + 30, 0


' --- 4. Display results and save to disk ---
'Draw String (IMG_W + 20, IMG_H + 5), "Reconstructed Image"
'BSave "reconstructed_image.bmp", final_image

locate 5,1
print " Algorithm test at different scales"
print
print " Press any key to exit"

' Clean up memory
ImageDestroy(source_image)
ImageDestroy(final_image)

Sleep

