''
''  you can specify .bmp files here, otherwise demo will make a procedural image
''

declare sub add_file( as string )


dim shared as string  image_folder' :  image_folder = exepath + "\bitmaps"

add_file "ah.bmp"
add_file "j.bmp"
add_file "z.bmp"
add_file "t.bmp"

/'
  i call load_bitmaps() after FreeBASIC gfx init
  .. on with the show ! 
'/


/' -- Lossy Image Compression with Randomized Circles - 2026 March 17.u2  by dafhi

    My 7th attempt, initial rollout was by Gemini after i explained the algorithm
    
    one cool benefit from gemini's version:  error caculation is a projective measurement.
  
    before, i would measure error, write shape, measure again, then restore to prep next shape
  
    update:
    trading what looks like 35% quality reduction at 40% fewer bytes
    
    update (2026 Nov 25)
    quick tidy-up

'/

function bmp_load( filename As String ) as any ptr 'modified from fb doc
    Dim As Long filenum = FreeFile()
    dim as long _w, _h '' 2021 Dec 19
    dim as any ptr  im
    for i as long = 1 to 2
        If Open( filename For Binary Access Read As #filenum ) = 0 Then
            Get #filenum, 19, _w
            Get #filenum, 23, _h
            close #filenum
            im = imagecreate( _w, _h )
            bload filename, im
            exit for
        endif
        Close #filenum
        filename = exepath & "\" & filename
    next
    return im
End function


Type imvars
    As Long      w, h, bypp, pitch
    As Any Ptr   pixels, im
End Type

  ' -- image class helpers
  sub _gfx_release( byref im as any ptr )
    if im <> 0 then if imageinfo(im) = 0 then imagedestroy im
    im = 0
  end sub

Sub fill_imvars( Byref i As imvars, im As Any Ptr = 0, free_mem__previous as boolean = true )
    if free_mem__previous then _gfx_release i.im
    If im = 0 Then
        ScreenInfo i.w, i.h, , i.bypp, i.pitch: i.pixels = screenptr
    Else
        ImageInfo im, i.w, i.h, i.bypp, i.pitch, i.pixels: i.im = im
    End If
End Sub


    dim shared as imvars imv_src()
    dim shared as string file_list()
    dim shared as long   image_index, u_imv = -1, u_filelist = -1

sub add_file( filename as string )
    u_filelist += 1
    redim preserve file_list( 1.5 * (u_filelist + 1) )
    file_list(u_filelist) = filename
end sub

sub load_bitmaps
    for i as long = 0 to u_filelist
        dim as any ptr q = bmp_load( image_folder + "\" + file_list(i) )
        if q then
            u_imv += 1
            redim preserve imv_src( 1.5 * (u_imv + 1) )
            fill_imvars imv_src(u_imv), q
        else
            u_filelist -= 1
        endif
    Next
End Sub


'' 2025 Oct 29
function min( a as double, b as double ) as double: if a<b then return a
return b :  end function
function max( a as double, b as double ) as double: if a>b then return a
return b :  end function

function clamp( in As double, hi As double = 1, lo As double = 0) As double
  return min( max(in, lo), hi )
End Function

function sqr_safe( d as double ) as double
    return sgn(d) * sqr( abs(d))
end function

    Namespace custom_rng
  Const As ULongInt mulA = &b0000000001000000000100000000100000001000000100000100001000100101 '' 2026 March 13
  Const As ULongInt xB = &b1011001001010101100000100010111011000001000011110110001000111001 '' 2026 March 17
      Dim As ULongInt a
Function v As ULongInt
    a = (mulA) * a + 1 '' 2026 March 16
    return a
End Function
End Namespace

function f_rng as single
    dim as ulongint a = custom_rng.v
    return (a shr 32) / ( 2^32 + (128) )
end function

  #define rng  f_rng


    Namespace hashed_shapes_single_channel

Type CircleShape
    as single   radius, x, y
    As long     brightness ' Can be positive or negative
End Type
    Dim As short accumulator_buffer(any, any)
    dim as long  pos_neg, g_cbits_total, current_channel_idx, g_cbits, cbits_base
    dim as long  x0, x1, y0, y1, ix0, ix1

sub _cliprect( c as CircleShape, imv as imvars )
    x0 = max( c.x - c.radius, 0 )
    y0 = max( c.y - c.radius, 0 )
    x1 = min( c.x + c.radius, imv.w - 1 )
    y1 = min( c.y + c.radius, imv.h - 1 )
end sub

Sub setup_channel_processing( imva as imvars, im As Any Ptr, channel_idx As Integer, release_imv_image as long = true )
    if imva.im <> im then fill_imvars( imva, im, release_imv_image )
      
        dim as long wa = ubound(accumulator_buffer,1) + 1
        dim as long ha = ubound(accumulator_buffer,2) + 1
    if imva.w<>wa or imva.h<>ha then redim accumulator_buffer(imva.w-1,imva.h-1)
    
    current_channel_idx = channel_idx
End Sub

    Dim As imvars     source_imv
  
Sub render_circle_to_accum_buf( c As CircleShape, seed as long, reconstruction_phase as long = false )
    if seed = 0 then exit sub
    
    _cliprect c, source_imv
    pos_neg = iif(reconstruction_phase, -c.brightness, c.brightness)
    Dim As single r_squared = c.radius * c.radius

        For y As Long = y0 To y1
    Dim As single dy = y - c.y
    dim as single r     = sqr_safe(r_squared - dy*dy) - .5 '  .. i forget why 
        For x As Long = max( c.x - r, x0 ) To min( c.x + r, x1 )
    #if 1
    accumulator_buffer(x,y) += pos_neg
    #else
    ' Instead of binary +brightness/-brightness
    ' Use a weight based on distance
    Dim As Single distSq = (x - c.x)^2 + (y - c.y)^2
    Dim As Single weight = exp(-distSq / (c.radius * c.radius * 2)) ' Adjust the divisor for softness
    accumulator_buffer(x,y) += pos_neg * weight
    #endif
    Next x
    Next y
    
End Sub

Function fitness_of_circle(c As CircleShape) As double
    _cliprect c, source_imv: dim as double score
    Dim As single r_squared = c.radius ^ 2, inv_rSquared = 1 / r_squared

    '' sparse sampling for much faster encodes
    dim as long _step = max( sqr(c.radius * .3), 1 )
    dim as long checkerboard_bit = iif(_step > 1, 1, 0)
    
        For y As Long = y0 To y1 step _step
    dim as single dy = y - c.y, dySq = dy * dy
    dim as single r   = sqr_safe( r_squared - dySq ) - .5 ' circle-constrained scanline by dafhi.  he forgets why the - .5
    dim as long o = (y \ _step) and checkerboard_bit '' 2025 Oct 29 - checker offset works now.
    For x As Long = max( x0 + o, c.x - r ) To min( c.x + r, x1 ) step _step
        dim as single er0 = accumulator_buffer(x,y) '' image information represented by accumulation buffer hills and valleys.  Gemini called this residual processing
        dim as single er1 = accumulator_buffer(x,y) + c.brightness
        dim as single improvement = er0*er0 - er1*er1
        score += improvement' * (2.5 + (dySq + (x - c.x) ^ 2) * inv_rSquared)
    Next x
    Next y
    
    Return score * _step * _step / 99999
End Function

    dim as ulongint index_based_rng_state, g_shape_index, low_perf_shapes

    function f_radius() as double
        dim as single f = (1 / (1 + g_shape_index * .02)) ^ 1.75
        return 1.75 * (1-f) + (f)*(source_imv.w + source_imv.h) / 4.5
    end function

    sub shape_props_from_seed( byref c as CircleShape, seed as long )
        using custom_rng
        a = index_based_rng_state
        a xor= seed * mulA
        
        c.x = source_imv.w * rng
        c.y = source_imv.h * rng
    end sub
    
    sub shape_props_from_index( byref c as CircleShape, index as long )
        
        ' - independent -
        '
            using custom_rng
        a = current_channel_idx xor mulA
        a = (a + index) * xB
        g_shape_index = index
          pos_neg = iif( rng < .5, -1, 1 )
        c.brightness = pos_neg * 17 * (.4 + rng) '' slaps
        
        ' - dependent -
        '
        c.radius = f_radius
        g_cbits = cbits_base + 5.75 / (1 + index / 40)
        index_based_rng_state = a '' save the rng state
    
    End Sub

    dim As CircleShape  candidate, circ_out

Function f_best_shape_seed( shape_index as long ) As long
    shape_props_from_index candidate, shape_index
    g_cbits_total += g_cbits
    
    Dim As double best_score = -1e12
    dim as long best_seed
    
    For seed As long = 1 To 2^g_cbits - 1
        shape_props_from_seed candidate, seed
        Dim As double score = fitness_of_circle(candidate)
        If score > best_score Then
            best_score = score
            best_seed = seed
        End If
    Next
    '? best_score; " ";
    if best_score <= 0 then best_seed = 0
    
    Return best_seed
End Function

End Namespace ' -- hashed_shapes_single_channel


Namespace orchestrator

    dim as long   u, seeds(), component_average = 127
    dim as imvars imv_out

    using hashed_shapes_single_channel

Sub encode( source_image as any ptr, channel_idx As long, num_circles As long )

    dim as long retain_source = true
    setup_channel_processing( source_imv, source_image, channel_idx, not retain_source )
    
    ' --- copy source channel and subtract the average
    var src_pix = Cptr(UByte Ptr, source_imv.pixels)
    
    For y As Long = 0 To source_imv.h-1
        var row_ptr = src_pix + y * source_imv.pitch
    For x As Long = 0 To source_imv.w-1
        accumulator_buffer(x,y) = row_ptr[x * 4 + channel_idx] - component_average
    Next: next
    
    u = num_circles:  if u > ubound(seeds) then redim seeds(u)
    For i As long = 1 To u
        seeds(i) = f_best_shape_seed(i)
        shape_props_from_index circ_out, i
        shape_props_from_seed circ_out, seeds(i)
        render_circle_to_accum_buf circ_out, seeds(i)
    Next i

End Sub

Sub decode( channel_idx As Integer, imv_result as imvars )
    
    setup_channel_processing( imv_result, imv_result.im, channel_idx)

        For y As Long = 0 To imv_result.h - 1
    For x As Long = 0 To imv_result.w - 1
    accumulator_buffer(x,y) = component_average
    Next : next

    For i As Integer = 1 to u
        shape_props_from_index circ_out, i
        shape_props_from_seed circ_out, seeds(i)
        dim as long reconstruction = true
        render_circle_to_accum_buf circ_out, seeds(i), reconstruction
        low_perf_shapes -= seeds(i) = 0
    Next i

    ' copy to corresponding target channel
    var dest_pixels = Cptr(UByte Ptr, imv_result.pixels)
    For y As Long = 0 To imv_result.h - 1
        Dim As UByte Ptr row_ptr = dest_pixels + y * imv_result.pitch
        For x As Long = 0 To imv_result.w - 1
            row_ptr[x * 4 + channel_idx] = clamp( accumulator_buffer(x,y), 255 )
        Next
    Next

End Sub
  sub set_local_imv( des as imvars, reference as imvars)
        if des.w <> reference.w or des.h <> reference.h then _
      fill_imvars des, imagecreate(reference.w, reference.h, rgb(0,0,0))
  end sub

sub entire_one_channel( src as any ptr, chan_idx as long, num_circles as long )
    encode src, chan_IDX, num_circles
      set_local_imv imv_out, source_imv
    decode chan_IDX, imv_out
end sub

End Namespace ' -- orchestrator

  using orchestrator
  

sub print_info( x as long, y as long, col as ulong )
    dim as long estimated_fileheader_size = 12
    
    Draw String (x + 10, y + 5), str(u ) + " circles per channel .. " _
    + str(estimated_fileheader_size + (g_cbits_total + 7)shr 3) + " bytes total", col
    
    Draw String (x + 10, y + 15), "cbits_base " + str(cbits_base), col
    locate 6,1
    dim as string helptext_images_arrows_LR
    if u_imv < 0 then helptext_images_arrows_LR = " left right : image"
    print "  Keys: " + chr(10) + _
    "                  " + chr(10) + _
    helptext_images_arrows_LR + chr(10) + _
    " up down    : bit depth"
    windowtitle "low performing shape count " + str(low_perf_shapes)
end sub


sub test( source_image as any ptr, num_circles as long, cbits_base_per_shape as long, x as long = 0, y as long = 0 )

    Const BLUE_IDX = 0, GREEN_IDX = 1, RED_IDX   = 2 ' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
    
    cbits_base = cbits_base_per_shape
    g_cbits_total = 0
    low_perf_shapes = 0
    entire_one_channel source_image, RED_IDX, num_circles
    entire_one_channel source_image, Green_IDX, num_circles
    entire_one_channel source_image, Blue_IDX, num_circles

    Put (x, y), imv_out.im, PSet
    
    print_info x+1, y+1, rgb(0,0,0)
    print_info x+1, y-0, rgb(0,0,0)
    print_info x+0, y+0, rgb(255,255,255)
end sub

Function modu(v As Double, m As Double = 1) As Double
    Dim As Single f = v / m
    Dim As Double si = int(f)
    Return m * 0.9999998 * (f - si)
End Function

' =============================================================================
'   MAIN PROGRAM
' =============================================================================

sub procedural_test_pattern( imv as imvars )
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

sub show_original
    dim byref as imvars imv = imv_src(image_index)
    cls
    
    Put (imv.w + 40, 10), imv.im, PSet
    Draw String (imv.w + 60, 25), "Original Image"
        
End Sub


screenres 400,300,32

load_bitmaps

dim as long   bit_depth = 6

dim as long   circles_per_channel = 300
dim as long   new_generation = true

  '' if no images make something
if u_imv < 0 then
    redim imv_src(0)
    fill_imvars imv_src(0), imagecreate( 400, 400, rgb(0,0,0) )
    procedural_test_pattern imv_src(0)
    u_imv = 0
EndIf


    dim as long max_w, max_h
    for i as long = 0 to u_imv
        max_w = max( imv_src(i).w, max_w )
        max_h = max( imv_src(i).h, max_h )
    Next

ScreenRes max_W * 2 + 30, max_H + 20, 32

dim as string kstr = ""
  #define extchar chr(255) '' L Shift, Alt, etc.

do
    select case (kstr)
    
    case chr(27) '' esc
        exit do
    
    case extchar + chr(72) ' - UP -
        if bit_depth < 8 then bit_depth += 1: new_generation = true
        
    case extchar + chr(80) ' - DOWN -
        bit_depth = max( 1, bit_depth - 1): new_generation = true
        
    case extchar + chr(75) ' - LEFT -
        image_index = modu( image_index - 1, u_imv + 1 )
        new_generation = true
        
    case extchar + chr(77) ' - RIGHT -
        image_index = modu( image_index + 1, u_imv + 1 )
        new_generation = true
        
    end select
    
      if new_generation then
    show_original
    test imv_src(image_index).im, circles_per_channel, bit_depth, 20, 10
    new_generation = false
    endif
    
    sleep 1
    kstr = inkey
loop

/'
'/
