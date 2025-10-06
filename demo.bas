/' -- Lossy Image Compression with Randomized Circles - 2025 Oct 6  by dafhi

    Initial rollout by Gemini to get me inspired.
    
    one cool benefit from that version (July ish)
      fitness is an in-place before-and-after calc.
  
    before, i would write a shape to accum buf,
    error calc, then erase (to prep for next rnd shape)
  
      update:
    evolutionary training on an internal downscaled image
  
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
    if i.im<>im then _gfx_release i.im
    If im = 0 Then
        ScreenInfo i.w, i.h, , i.bypp, i.pitch: i.pixels = screenptr
    Else
        ImageInfo im, i.w, i.h, i.bypp, i.pitch, i.pixels: i.im = im
    End If
End Sub

#define min( a,b) iif( (a)<(b), (a), (b) )
#define max( a,b) iif( (a)>(b), (a), (b) )

sub downscale(byref dest as imvars, byref source as imvars )

    '' initial and most-incredible version by GPT-4o 
    '' slight reduced LOC by 3.5-Sonnet
    
    dim as long dest_w = dest.w, dest_h = dest.h
    dim as single scaleX = source.w / dest_w, scaleY = source.h / dest_h
    
    for y as long = 0 to dest_h - 1
        dim as ulong ptr desRow = dest.pixels + y * dest.pitch
        dim as single srcStartY = y * scaleY, srcEndY = (y + 1) * scaleY
        
        for x as long = 0 to dest_w - 1
            dim as single srcStartX = x * scaleX, srcEndX = (x + 1) * scaleX
            dim as single fr = 0, fg = 0, fb = 0, totalWeight = 0
            
            for srcY as integer = int(srcStartY) to min(int(srcEndY), source.h - 1)
                dim as ulong ptr srcRow = source.pixels + srcY * source.pitch
                dim as single yWeight = 1.0 - max(0, srcStartY - srcY) - max(0, srcY + 1 - srcEndY)
                
                for srcX as integer = int(srcStartX) to min(int(srcEndX), source.w - 1)
                    dim as single xWeight = (1.0 - max(0, srcStartX - srcX) - max(0, srcX + 1 - srcEndX)) * yWeight
                    dim as ulong pixel = srcRow[srcX]
                    
                    fr += ((pixel shr 16) and 255) * xWeight
                    fg += ((pixel shr 8) and 255) * xWeight
                    fb += (pixel and 255) * xWeight
                    totalWeight += xWeight
                next srcX
            next srcY
            
            desRow[x] = iif(totalWeight > 0, rgb(fr / totalWeight, fg / totalWeight, fb / totalWeight), 0)
        next x
    next y
end sub

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
End Type


type t_hyper_parameter
    declare constructor( as string = "" )
    declare operator cast as single
    declare operator cast as string
    declare operator let( as single )
    declare sub mutate( as single = 1, as single = 0 )
    as string name = ""
    as single f
end type
constructor t_hyper_parameter( nam as string ): name = nam : end constructor
operator t_hyper_parameter.cast as single : return f: end operator
operator t_hyper_parameter.cast as string : return name + " " + str(f): end operator
operator t_hyper_parameter.let( _f as single ): f = _f: end operator
sub t_hyper_parameter.mutate( hi as single, lo as single )
    dim as single a = lo + rnd * (hi - lo), b = hi + rnd * (lo - hi) '' experimental
    f = a+rnd*(b-a)
end sub

    sub print_param( t as t_hyper_parameter, x as long = 0, y as long = 0, col as ulong = rgb(255,255,255) )
        draw string (x,y), t.name + " " + round(t.f, 3), col
    end sub


type hyper_parameters

    /'  code example : loop through all params

    forloop_hparams( @hypers ) ' <- included macro
      print *p                 ' <- param
    next
    '/

        #define forloop_hparams(q)  _     ' ready-to-use macro
    for p as t_hyper_parameter ptr = _
    @(q).cbits_base to @(q).br_var     ' < -- make ends match first and last hyper


    as t_hyper_parameter cbits_base      = type( "bits base" )
    as t_hyper_parameter cbits_var       = type( "bits var" )
    as t_hyper_parameter cbits_var_decay = type( "bits var dcy" )
    as t_hyper_parameter div_rad_max     = type( "div rad max" )
    as t_hyper_parameter br_decay        = type( "br decay" )
    as t_hyper_parameter br_var_decay    = type( "br var dcy" )
    as t_hyper_parameter br_base         = type( "br base" )
    as t_hyper_parameter br_var          = type( "br var" )

    as single score, PSNR, mean_squared_error
    as long   cbits_total

    as ushort seeds(any)

end type

sub mutate( d as hyper_parameters ) '' reasonable values as 

    d.cbits_base.mutate 5, 7 '' base
    d.cbits_var.mutate 0, 5  '' variance .. (base + var.. should stay <= 10 for encode speed)

    d.cbits_var_decay.mutate .9999, .975

    d.div_rad_max.mutate 25, 5.5 '' lowest recommended value 4.0 ish for encode speed

    d.br_base.mutate 130, 40
    d.br_var.mutate 130, 40

    d.br_decay.mutate .9999, .975
    d.br_var_decay.mutate .9999, .975
    
    d.score = 0
end sub

  sub print_hypers( h as hyper_parameters )
      forloop_hparams( h )
      print *p : next
  end sub

Const BLUE_IDX  = 0 ' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
Const GREEN_IDX = 1
Const RED_IDX   = 2
Const ALPHA_IDX = 3
' ------------------------------


    Namespace hashed_shapes_encoder

Dim As ACCUM_TYPE accumulator_buffer(any, any)
dim as long       x0, x1, y0, y1, ix0, ix1, _dbg

  sub _cliprect( c as CircleShape, imv as imvars )
      x0 = max( c.x - c.radius, 0 )
      y0 = max( c.y - c.radius, 0 )
      x1 = min( c.x + c.radius, imv.w - 1 )
      y1 = min( c.y + c.radius, imv.h - 1 )
  end sub
    dim as long             pos_neg, component_average, current_channel_idx ' R,G,B
    dim as single           g_radius, hard_rad_max, hard_rad_min = 0.77
    dim as hyper_parameters hypers, h_backup

Sub setup_channel_processing( imv as imvars, channel_idx As Integer)
    redim accumulator_buffer(imv.w-1,imv.h-1)
    current_channel_idx = channel_idx
    hard_rad_max = (imv.w + imv.h) / hypers.div_rad_max
    g_radius = hard_rad_max
End Sub

Sub render_circle_to_accum_buf( im_src as imvars, c As CircleShape, reconstruction_phase as long = false )
    
    _cliprect c, im_src
    pos_neg = iif(reconstruction_phase, -c.brightness, c.brightness)
    Dim As single r_squared = c.radius * c.radius

        For y As Long = y0 To y1
    Dim As single dy = y - c.y
    dim as single r  = sqr_safe(r_squared - dy*dy) - .5 ' - .5 fixed an anti-aliasing project
        For x As Long = max( c.x - r, x0 ) To min( c.x + r, x1 )
    accumulator_buffer(x,y) += pos_neg
    Next x
    Next y
        g_radius = c.radius
  
End Sub

Function fitness_of_circle( im_src as imvars, c As CircleShape) As double
    
    var score = cdbl(0) : _cliprect c, im_src
    Dim As Long r_squared = c.radius * c.radius

      '' sparse samples
    dim as long _step = max( sqr(c.radius * .4), 1 )
    dim as long checker_xor = iif(_step > 1, 1, 0)
    
        For y As Long = y0 To y1 step _step
    dim as single r   = sqr_safe( r_squared - (y - c.y)*(y - c.y) ) - .5 ' circle constraints by dafhi.  he forgets why the - .5
        ix0 = c.x - r + (y and checker_xor)
        For x As Long = max( ix0, x0 ) To min( c.x + r, x1 ) step _step
    dim as single er0 = accumulator_buffer(x,y) '' accum_buffer holds all necessary info .. no source channel needed .. Gemini called this residual processing
    dim as single er1 = accumulator_buffer(x,y) + c.brightness
    dim as single improvement = er0*er0 - er1*er1
    score += improvement
    Next x
    Next y
    
    Return score * _step * _step / 1e3

End Function


  function f_cbits( shape_index as long ) as long
      return hypers.cbits_base + hypers.cbits_var * hypers.cbits_var_decay ^ (shape_index - 1)
  end function

  function f_radius( ) as double
      dim as single f = rng
      return clamp( g_radius * ( 1.45*f + 0.6*(1-f) ), hard_rad_max, hard_rad_min )
  end function
    
  function f_brightness_base_factor( shape_index as long ) as single
      return hypers.br_decay ^ (shape_index - 1)
  end function

  function f_brightness_variance_factor( shape_index as long ) as single
      return hypers.br_var_decay ^ (shape_index - 1)
  end function
    dim as single   br_base, br_var, xy_a, xy_b, bright, rad

  sub internal_vars_from_rng( shape_index as long)
      rad = f_radius
        br_base = f_brightness_base_factor( shape_index )
        br_var  = 1-f_brightness_variance_factor( shape_index )
        pos_neg = iif( rng < .5, -1, 1 )
      bright = pos_neg * ( hypers.br_base * br_base + rng * ( 1 + hypers.br_var * br_var ) )
      xy_a = rng
      xy_b = rng
  end sub

  sub shape_from_rng( imv as imvars, c as circleShape, shape_index as long )
    internal_vars_from_rng shape_index
    c.radius = rad
    c.brightness = bright
    c.x = imv.w * xy_a * 1.2 - .1
    c.y = imv.h * xy_b * 1.2 - .1
  end sub

    static As CircleShape best_fit, candidate
    dim as ushort         g_random_seed, best_seed
    dim as ulong          n_shapes, new_generation

    sub shape_props_from_seeds( im_src as imvars, c as CircleShape, shape_index as long, seed as long )
          using custom_rng
        a = (current_channel_idx xor g_random_seed) * mulB
        a xor= seed * mulA
        a xor= a shl 1
        a xor= shape_index * mulA
        shape_from_rng im_src, c, shape_index
    end sub

Function find_best_shape( im_src as imvars, shape_index as long ) As CircleShape
    Dim As double best_score = -1e12
    dim as long cbits = f_cbits(shape_index)

        For seed As long = 1 To 2 ^ cbits
      shape_props_from_seeds im_src, candidate, shape_index, seed
    Dim As double score = fitness_of_circle( im_src, candidate)
      If score > best_score Then
    best_score = score
    best_fit = candidate
    best_seed = seed
    End If
    Next

    hypers.cbits_total += cbits
    Return best_fit
End Function

      Dim As imvars   imv_result

  Sub _decode( target_image As Any Ptr, channel_idx As Integer )

      setup_channel_processing( imv_result, channel_idx)

          For i As Integer = 1 to n_shapes
      dim as long reconstruction = true
      shape_props_from_seeds imv_result, best_fit, i, hypers.seeds(i*3 + channel_idx)
      render_circle_to_accum_buf imv_result, best_fit, reconstruction
      Next i
      
      ' copy to corresponding target channel, save energy information for PSNR-like
      Dim As UByte Ptr dest_pixels = Cptr(UByte Ptr, imv_result.pixels)
          For y As Long = 0 To imv_result.h - 1
      Dim As UByte Ptr row_des = dest_pixels + y * imv_result.pitch + channel_idx
      For x As Long = 0 To imv_result.w - 1
      row_des[x*4] = clamp( accumulator_buffer(x,y) + component_average, 255 )
      Next
      Next
      
  End Sub
  
  Sub _encode( imv as imvars, channel_idx As Integer, num_circles As Integer )

      setup_channel_processing( imv, channel_idx )
      
        ' --- average channel brightness
      var src_pix = Cptr(UByte Ptr, imv.pixels)
      component_average = 127
      
      ' --- copy source channel and subtract the average
          For y As Long = 0 To imv.h-1
      Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
          For x As Long = 0 To imv.w-1
      accumulator_buffer(x,y) = _
      row_ptr[x * 4 + channel_idx] - component_average
      Next: next

      ' -- search
      For i As long = 1 To n_shapes
          best_fit = find_best_shape(imv, i)
          hypers.seeds(i*3 + channel_idx) = best_seed
          render_circle_to_accum_buf imv, best_fit
      Next i
      
      dim as longint delta_sum
      Dim As UByte Ptr src_pixels = Cptr(UByte Ptr, imv.pixels)
      For y As Long = 0 To imv.h - 1
          Dim As UByte Ptr row_src = src_pixels + y * imv.pitch + channel_idx
          For x As Long = 0 To imv.w - 1
              dim as long _result = clamp( accumulator_buffer(x,y), component_average, component_average - 255 )
              delta_sum += (_result)^2
          Next
      Next : hypers.mean_squared_error += delta_sum / imv.w / imv.h
      
  End Sub

sub decode( des as any ptr )
    _dbg = 1
    if imv_result.im <> des then  fill_imvars(imv_result, des)
    _decode(des, red_IDX )
    _decode(des, Green_IDX )
    _decode(des, Blue_IDX )
end sub

sub encode( src as any ptr, num_shapes as long )
    
    static as imvars  imv_small, imv
    
    if imv.im <> src then fill_imvars imv, src
    dim as single r = sqr(imv.w^2+imv.h^2)
    if r < 2 then exit sub
    
    dim as imvars ptr p_imv = @imv
    dim as single scale = min( 250 / r, 1 )': dim as imvars ptr p_imv = @imv
    if scale < 1 then
        fill_imvars imv_small, imagecreate( imv.w * scale, imv.h * scale )
        downscale imv_small, imv
        p_imv = @imv_small
    endif
    n_shapes = num_shapes
    
    dim as long u = n_shapes * 3 + 2 '' seeds( i*3 + channel_idx )
    if u > ubound(hypers.seeds) then redim hypers.seeds( u * 1.5 )
    
    if hypers.psnr = 0 then mutate hypers '' initialize

    hypers.mean_squared_error = 0
    hypers.cbits_total = 0
    
    _dbg = 0
    _encode(*p_imv, Red_IDX, n_shapes )
    _encode(*p_imv, Green_IDX, n_shapes )
    _encode(*p_imv, Blue_IDX, n_shapes )

    hypers.psnr = 1e6 / ( hypers.mean_squared_error + 1 )
    
    hypers.score = 1e3 * hypers.psnr / (1 + hypers.cbits_total ^ 1.5)
    
    new_generation = true '' demo
end sub

End Namespace ' -- hashed_shapes_encoder


sub testpattern( imv as imvars )
    dim as long seed = 82, w = imv.w, h = imv.h, diagonal = sqr(w*w + h*h)
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

    using hashed_shapes_encoder
  sub print_info( ix as long, iy as long, col as ulong )
      dim as long future_header_size = 12
      draw string (ix + 180, iy + 0), "bytes " + str(future_header_size + (hypers.cbits_total + 7)shr 3),col
      draw string (ix, iy + 45), "score (PSNR, bits) " + round( hypers.score, 2),col
      draw string (ix, iy + 55), "PSNR " + round( hypers.psnr, 2),col
      print_param hypers.br_base, ix, iy + 10, col
      print_param hypers.br_var, ix, iy + 20, col
      print_param hypers.div_rad_max, ix, iy + 30, col
  end sub

sub show( des as any ptr, ix as long=0, iy as long=0 )
    if ubound(hypers.seeds) < 0 then exit sub
    decode des
    Put (ix, iy), des, PSet
    print_info ix+1, iy+1, rgb(0,0,0)
    print_info ix, iy, rgb(255,255,255)
end sub

sub evolve( src as any ptr, n_shapes as long = 100, u_loop as long = 0 )
locate 1,1
? "scores from random params"
        for i as long = 0 to u_loop
    h_backup = hypers
    mutate hypers
    encode src, n_shapes
    locate i,30
    ? hypers.score
    if h_backup.score > hypers.score then hypers = h_backup
    next
end sub


Const IMG_W = 400
Const IMG_H = 350

dim as long shapes_per_chan = 1

ScreenRes IMG_W * 2 + 30, IMG_H + 20, 32

const black = rgb(0,0,0)

Dim As Any Ptr source_image = ImageCreate(IMG_W, IMG_H, 0, Black)

dim as imvars imv : fill_imvars imv, source_image
testpattern imv
Put (10, 10), source_image, PSet
encode source_image, shapes_per_chan

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)

  #define extchar chr(255) '' L Shift, Alt, etc.
dim as string kstr = ""
dim as single help_time = 4

dim as double t = timer, dt, tp, t0 = t

randomize

do
    tp = t : t = timer
    dt = t - tp
    
    select case (kstr)
    case ""
    case "a", "A", "z"
        evolve source_image, shapes_per_chan, clamp( 19 / (shapes_per_chan)^.33, 10, 1 )
    case extchar + chr(75) ' - LEFT -
        if shapes_per_chan > 1 then
        shapes_per_chan -= max(1, 0.2*shapes_per_chan )
        encode source_image, shapes_per_chan
        endif
    case extchar + chr(77) ' - RIGHT -
        if shapes_per_chan < 999 then
        shapes_per_chan += max(1, 0.2*shapes_per_chan)
        encode source_image, shapes_per_chan
        endif
    case "h"
        help_time = t + 10 - t0
    case chr(27) '' esc
        exit do
    end select
    
      if new_generation then
    show final_image, IMG_W + 20, 10
    new_generation = false
    endif
    
    if t - t0 < help_time then
    locate 4,1
    print "  Keys: " + chr(10) + _
    "                  " + chr(10) + _
    "  left right : circle count" + chr(10) + _
    "  a          : params evolve"
    endif
    
    sleep 1
    kstr = inkey
loop

ImageDestroy(source_image)
ImageDestroy(final_image)

#if 0
chdir exepath
open "hypers.txt" for output as #1
  print #1 ,, "value", "max", "min"
  forloop_hparams( tile(0,0) )
    print #1, p->name, p->f, p->hi, p->lo
  next
close #1
#endif
