/' -- Lossy Image Compression with Randomized Circles - 2025 Oct 20  by dafhi

    Initial rollout by Gemini to get me inspired.
    
    one cool benefit from that version (July ish)
      fitness is an in-place before-and-after calc.
  
    before, i would write a circl to accum buf,
    error calc, then erase (to prep for next rnd circl)
  
      update:
    tweaks, code shuffle
  
'/


  ' -- image class helper
  sub _gfx_release( byref im as any ptr )
    if im <> 0 then if imageinfo(im) = 0 then imagedestroy im
    im = 0
  end sub

' -- image class
Type imvars
    As Long     w, h, bypp, pitch
    As Any Ptr  pixels, im
    declare     destructor
End Type
  destructor imvars
      _gfx_release im
  end destructor


' -- image class helper
Sub fill_imvars( Byref i As imvars, im As Any Ptr = 0)
    if i.im<>im then _gfx_release i.im
    If im = 0 Then
        ScreenInfo i.w, i.h, , i.bypp, i.pitch: i.pixels = screenptr
    Else
        ImageInfo im, i.w, i.h, i.bypp, i.pitch, i.pixels: i.im = im
    End If
End Sub

sub testpattern( imv as imvars )
    dim as long seed = 95, w = imv.w, h = imv.h, diagonal = sqr(w*w + h*h)
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

function clamp( in As double, hi As double = 1, lo As double = 0) As double '' ai code assistent suggested this is potentially slow
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


type t_hyper_parameter
    declare constructor( as string = "", as single = 0 )
    declare operator cast as single
    declare operator cast as string
    declare operator let( as single )
    declare sub mutate( as single = 1, as single = 0 )
    as string name = ""
    as single f
end type
constructor t_hyper_parameter( nam as string, v as single ): name = nam : f = v : end constructor
operator t_hyper_parameter.cast as single : return f: end operator
operator t_hyper_parameter.cast as string : return name + " " + str(f): end operator
operator t_hyper_parameter.let( v as single ): f = v : end operator
sub t_hyper_parameter.mutate( hi as single, lo as single )
    dim as single a = lo + rnd * (hi - lo), b = hi + rnd * (lo - hi) '' experimental
    #if 0
    a = a + rnd * (b-a)
    f = f + rnd * (a - f)
    #else
    f = a + rnd * (b-a)
    #endif
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
    @(q).div_rad_ini to @(q).br           ' < -- make ends match first and last hy param


    as t_hyper_parameter div_rad_ini      = type( "div rad ini", 15.3 )
    as t_hyper_parameter rad_decay        = type( "rad decay", .2 )
    as t_hyper_parameter br_var_decay     = type( "br var dcy", .97 )
    as t_hyper_parameter br_var           = type( "br var", 49 )
    as t_hyper_parameter br_decay         = type( "br decay", .999 )
    as t_hyper_parameter br               = type( "br", 29 )

    as single score, PSNR, mean_squared_error
    as long   cbits_total

end type

sub mutate( d as hyper_parameters ) '' reasonable values as 

    d.div_rad_ini.mutate 6.5, 23 '' lowest recommended value 4.0 ish for encode speed

    d.rad_decay.mutate .26, .15 '' division-based decay in f_radius()

    d.br.mutate 25, 60
    d.br_decay.mutate .9999, .95
    
    d.br_var.mutate 35, 85
    d.br_var_decay.mutate .9999, .95
    
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


    Namespace hashed_circles_encoder
    
dim as single           g_radius, hard_rad_max
dim as long             x0, x1, y0, y1, pos_neg, chan_idx, component_average, ix0, ix1, new_generation, _dbg

Dim As ACCUM_TYPE       accumulator_buffer(any, any)
dim as hyper_parameters hypers, h_backup
dim as long             n_groups, hdr_cbits = 2, shape_cbits = 4

type CircleShape
    as single x, y, rad, bright
end type

  sub _cliprect( imv as imvars, circl as CircleShape )
      x0 = max( circl.x - circl.rad, 0 )
      y0 = max( circl.y - circl.rad, 0 )
      x1 = min( circl.x + circl.rad, imv.w - 1 )
      y1 = min( circl.y + circl.rad, imv.h - 1 )
  end sub

    '' fixed bits header (perhaps vari width in future version)
    '' vari bits seed
    dim as ushort a_hdr( any, any ), a_seed( any, any ) '' ( val, rgb_channel )

Sub setup_channel_processing( imv as imvars, num_groups as ushort, channel_idx as long )
    redim accumulator_buffer(imv.w-1, imv.h-1)
    n_groups = num_groups
        dim as long u = ubound( a_hdr, 1 ), n = (n_groups + 1) * 1.3
    if n_groups > u then redim preserve a_hdr( n, 0 to 2 ), a_seed( n, 0 to 2 )
    hard_rad_max = (imv.w + imv.h) / hypers.div_rad_ini
    g_radius = hard_rad_max
    chan_idx = channel_idx
End Sub
    
Sub render_circle_to_accum_buf( im_src as imvars, circl as CircleShape, reconstruction_phase as long = false )

    _cliprect im_src, circl
    pos_neg = iif(reconstruction_phase, -circl.bright, circl.bright)
    Dim As single r_squared = circl.rad * circl.rad

    For y As Long = y0 To y1
        Dim As single dy = y - circl.y, r = sqr_safe(r_squared - dy*dy) - .5 ' - .5 fixed an anti-aliasing project
        For x As Long = max( circl.x - r, x0 ) To min( circl.x + r, x1 )
        accumulator_buffer(x,y) += pos_neg
        Next x
    Next y

End Sub

Function fitness_of_circle( im_src as imvars, circl as CircleShape ) As double
    
    _cliprect im_src, circl: dim as double score
    Dim As Long r_squared = circl.rad * circl.rad

      '' sparse samples
    dim as long _step = max( sqr(circl.rad * .5), 1 )
    dim as long checkerboard_bit = iif(_step > 1, 1, 0)
    
        For y As Long = y0 To y1 step _step
    dim as single r   = sqr_safe( r_squared - (y - circl.y)*(y - circl.y) ) - .5 ' circle-constrained scanline by dafhi.  he forgets why the - .5
        ix0 = circl.x - r + (y and checkerboard_bit)
        For x As Long = max( ix0, x0 ) To min( circl.x + r, x1 ) step _step
    dim as single er0 = accumulator_buffer(x,y) '' accum_buffer holds all necessary info .. no source channel needed .. Gemini called this residual processing
    dim as single er1 = accumulator_buffer(x,y) + circl.bright
    dim as single improvement = er0*er0 - er1*er1
    score += improvement
    Next x
    Next y
    
    Return score * _step * _step

End Function

    dim as ushort     g_random_seed
    dim as single     xy_a, xy_b, br_var, hard_rad_min = 0.77

  function f_radius( circl_index as long ) as double
      dim as single f = rng, j = 1 + .07 + .29 / (1 + circl_index * hypers.rad_decay)
      return clamp( g_radius * ( (2-j)*f + j*(1-f) ), hard_rad_max, hard_rad_min )
  end function
    
  function f_brightness( circl_index as long ) as single
      return hypers.br * hypers.br_decay ^ (circl_index - 1)
  end function

  function f_brightness_variance( circl_index as long ) as single
      return hypers.br_var * hypers.br_var_decay ^ (circl_index - 1)
  end function

    sub rng_from_seeds( circle_index as long, seed as long, parent_rngstate as ulongint = 0 )
          using custom_rng
          a = seed * mulA
          a = (a xor circle_index) * mulA
          a = (a xor chan_idx) * mulA
          a = (a xor parent_rngstate xor g_random_seed) * mulB
    end sub

        dim as ulongint     group_rng_state, g_grp_idx
        dim as CircleShape  best_fit, circ

    sub circle_props( imv as imvars, circle_idx as long, seed_circs as ushort)
        circle_idx += g_grp_idx
        rng_from_seeds circle_idx, seed_circs, group_rng_state
        
          br_var  = f_brightness_variance( circle_idx )
          pos_neg = iif( rng < .5, -1, 1 )
        circ.bright = pos_neg * ( 1 + rng + f_brightness( circle_idx ) + rng * br_var )
        
        circ.x = imv.w * (rng * 1.1 - .05)
        circ.y = imv.h * (rng * 1.1 - .05)
        circ.rad = f_radius( circle_idx )
    end sub
    
        dim as long   cbits_circles, best_seed, best_grp_cbits, i_hy_raw
    
    sub group_derivates( hdr as ushort, grp_idx as long )
        rng_from_seeds( grp_idx, hdr, 0 )
        cbits_circles = rng * (shape_cbits + .5)' + .25
        group_rng_state = custom_rng.a
        g_grp_idx = grp_idx
    end sub
    
    sub draw_circle( im_src as imvars, seed_circs as ushort, decode_stage as long = true )
        circle_props im_src, 0, seed_circs
        render_circle_to_accum_buf im_src, circ, decode_stage
    end sub

    function f_group_score( im_src as imvars, hdr as ubyte, idx as long ) as double
        
        group_derivates hdr, idx
        dim as double best_score = -1e12

            for seed_circs as ushort = 0 to 2^cbits_circles - 1
        circle_props im_src, 0, seed_circs
        dim as double score = fitness_of_circle( im_src, circ )
        if score > best_score then best_score = score : best_seed = seed_circs
        next
        return best_score
    end function
    
    sub find_best_hdr( im_src as imvars, idx as long )
        dim as double   best_score = -1e12
            for hdr as ushort = 0 to 2^hdr_cbits - 1
                const as single cb_penalty_shape = 5/6
        dim as single score = f_group_score( im_src, hdr, idx ) / log(1+hdr_cbits + cbits_circles)' ^ cb_penalty_shape
            If score > best_score Then
        best_score = score
        a_hdr(idx, chan_idx) = hdr
        a_seed(idx, chan_idx) = best_seed
        best_grp_cbits = hdr_cbits + cbits_circles
        endif
        next
        hypers.cbits_total += best_grp_cbits
    End sub

    sub convert_accum_to_imvars( imv as imvars )
        Dim As UByte Ptr dest_pixels = Cptr(UByte Ptr, imv.pixels)
        For y As Long = 0 To imv.h - 1
        Dim As UByte Ptr row_des = dest_pixels + y * imv.pitch + chan_idx
            For x As Long = 0 To imv.w - 1
            row_des[x*4] = clamp( accumulator_buffer(x,y) + component_average, 255 )
            Next
        Next y
    end sub

        Dim As imvars   imv_result

  Sub _decode( target_image As Any Ptr, chan_idx As long)
      setup_channel_processing imv_result, n_groups, chan_idx
      For i As long = 1 to n_groups
          group_derivates a_hdr( i, chan_idx ), i
          draw_circle imv_result, a_seed(i, chan_idx)
          g_radius = circ.rad
      Next
      convert_accum_to_imvars imv_result
  End Sub
  
  Sub _encode( imv as imvars, chan_idx As long, num_groups As long )

      setup_channel_processing imv, num_groups, chan_idx
      
        ' --- average channel brightness
      var src_pix = Cptr(UByte Ptr, imv.pixels)
      component_average = 127
      
      ' --- copy source channel and subtract the average
          For y As Long = 0 To imv.h-1
      Dim As UByte Ptr row_ptr = src_pix + y * imv.pitch
          For x As Long = 0 To imv.w-1
      accumulator_buffer(x,y) = row_ptr[x * 4 + chan_idx] - component_average
      Next: next

      ' -- search
      For i As long = 1 To num_groups
          find_best_hdr imv, i
          group_derivates a_hdr(i, chan_idx), i
          const decode_stage = false
          draw_circle imv, a_seed(i, chan_idx), decode_stage
          g_radius = circ.rad
      Next i
      
      dim as longint delta_sum
      Dim As UByte Ptr src_pixels = Cptr(UByte Ptr, imv.pixels)
      For y As Long = 0 To imv.h - 1
          Dim As UByte Ptr row_src = src_pixels + y * imv.pitch + chan_idx
          For x As Long = 0 To imv.w - 1
              dim as long _result = clamp( accumulator_buffer(x,y), component_average, component_average - 255 )
              delta_sum += (_result)^2
          Next
      Next : hypers.mean_squared_error += delta_sum / imv.w / imv.h
      
  End Sub

sub decode( des as any ptr )
    if imv_result.im <> des then  fill_imvars(imv_result, des)
    _decode(des, red_IDX )
    _decode(des, Green_IDX )
    _decode(des, Blue_IDX )
end sub
    
    dim as imvars  imv_small, imv

    sub choose_whether_to_downscale( r as single, byref p as imvars ptr, internal_image_diagonal as single )
        dim as single scale = min( internal_image_diagonal / r, 1 )
        if scale < 1 then
            fill_imvars imv_small, imagecreate( imv.w * scale, imv.h * scale )
            downscale imv_small, imv
            p = @imv_small
        endif
    end sub

sub encode( src as any ptr, n_groups as long )
    
    if imv.im <> src then fill_imvars imv, src
    
    dim as single r = sqr(imv.w^2+imv.h^2)
    if r < 2 then exit sub
    
    dim as imvars ptr p_imv = @imv
    choose_whether_to_downscale r, p_imv, 150 '' scaled diagonal
    
    hypers.mean_squared_error = 0
    hypers.cbits_total = 0
    
    _encode *p_imv, red_IDX, n_groups
    _encode *p_imv, green_IDX, n_groups
    _encode *p_imv, blue_IDX, n_groups
        
        const as single cb_penalty_image = 9/6
    hypers.psnr = 1e6 / ( hypers.mean_squared_error + 1 )
    hypers.score = 1e2 * hypers.psnr / (0+hypers.cbits_total) ^ cb_penalty_image
    
    new_generation = true
end sub

    const  u_hy = 2
    dim as hyper_parameters hy(u_hy)

End Namespace ' -- hashed_circls_encoder

' =============================================================================
'   MAIN PROGRAM
' =============================================================================

  using hashed_circles_encoder
  
  sub print_info( ix as long, iy as long, col as ulong )
      draw string (ix + 180, iy + 0), "bytes " + str((hypers.cbits_total + 7)shr 3),col
      print_param hypers.br, ix, iy + 10, col
      print_param hypers.br_decay, ix, iy + 20, col
      print_param hypers.div_rad_ini, ix, iy + 30, col
      print_param hypers.rad_decay, ix, iy + 40, col
      draw string (ix, iy + 55), "shape cbits " + str(shape_cbits) + "  used in group_derivates", col
      draw string (ix, iy + 75), "score (PSNR, bits) " + round( hypers.score, 2),col
      draw string (ix, iy + 85), "PSNR " + round( hypers.psnr, 2),col
  end sub

sub show( des as any ptr, ix as long=0, iy as long=0 )
    decode des
    Put (ix, iy), des, PSet
    print_info ix+1, iy+1, rgb(0,0,0)
    print_info ix, iy, rgb(255,255,255)
end sub

    sub calculate_internal_array_scores( src as any ptr, n_groups as long )
        for i as long = 0 to u_hy
            hypers = hy(i)
            encode src, n_groups '' update with new settings
            hy(i).score = hypers.score
        next
    end sub


sub _evolve( src as any ptr, user_cbits_adjust as long, n_groups as long = 100, loops as long = 1 )
    
    shape_cbits = user_cbits_adjust
    
        for i as long = 1 to loops
    dim as long n = int(rnd*(u_hy+1))
    hypers = hy(n)
    mutate hypers
    encode src, n_groups
    locate 1,30
    ? hypers.psnr, hypers.score
    if hypers.score > hy(n).score then hy(n) = hypers
    sleep 1
    next
    
end sub

sub evolve( src as any ptr, n_groups as long = 100, u_loop as long = 0 )
    dim as long cbits_save = shape_cbits
    
    h_backup = hypers
    g_random_seed = rnd * culng(-1)
    calculate_internal_array_scores src, n_groups
    
    locate 1,1
    ? "scores from random params"
    
    if shape_cbits > 0 then
    '_evolve src, cbits_save-1, n_groups, (u_loop + 1) / 2
    endif
    _evolve src, cbits_save, n_groups, (u_loop + 1)' / 1.5
    
    for i as long = 0 to u_hy
        if hy(i).score > hypers.score then  hypers = hy(i)
    next
    if h_backup.score > hypers.score then  hypers = h_backup
    encode src, n_groups '' update with new settings

end sub

    randomize


Const IMG_W = 400
Const IMG_H = 350

dim as long shapes_per_chan = 25

ScreenRes IMG_W * 2 + 30, IMG_H + 20, 32

const black = rgb(0,0,0)

Dim As Any Ptr source_image = ImageCreate(IMG_W, IMG_H, 0, Black)

dim as imvars imv : fill_imvars imv, source_image
testpattern imv
Put (10, 10), source_image, PSet
encode source_image, shapes_per_chan

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)
show final_image, IMG_W + 20, 10

dim as long keystate_A, keystate_0_A
dim as long keystate_S, keystate_0_S
dim as long keystate_CTRL, keystate_0_CTRL, keystate_allow_filesave

  #define extchar chr(255) '' L Shift, Alt, etc.
dim as string kstr = ""
#if 1
    locate 4,1
    print "  Keys: " + chr(10) + _
    "                  " + chr(10) + _
    "  left right : circle count" + chr(10) + _
    "  a          : evolve hyperparameters" + chr(10) + _
    "  g          : global seed"
#endif
dim as double t = timer, dt, tp, t0 = t

chdir exepath

do

    tp = t : t = timer
    dt = t - tp
    
    keystate_0_A = keystate_A:    keystate_A = multikey(30)
    keystate_0_S = keystate_S:    keystate_S = multikey(31)
    
    keystate_0_CTRL = keystate_CTRL:    keystate_CTRL = multikey(29)
    
    if keystate_A <> 0 and keystate_0_A = 0 then
        evolve source_image, shapes_per_chan, clamp( 9999 / (shapes_per_chan * shape_cbits)^.75, 25, 3 )
    endif
    
    select case (kstr)
    case chr(27) '' esc
        exit do
    case "g", "G"
        g_random_seed = rnd * culng(-1)
        encode source_image, shapes_per_chan
    case extchar + chr(75) ' - LEFT -
        if shapes_per_chan > 1 then
        shapes_per_chan -= max(1, 0.2*shapes_per_chan )
        encode source_image, shapes_per_chan
        endif
    case extchar + chr(77) ' - RIGHT -
        if shapes_per_chan < 1999 then
        shapes_per_chan += max(1, 0.2*shapes_per_chan)
        encode source_image, shapes_per_chan
        endif
    end select
    
      if new_generation then '' new_generation set True in encode sub
    show final_image, IMG_W + 20, 10
    new_generation = false
    endif
    
    if (keystate_0_CTRL = 0) and keystate_CTRL then keystate_allow_filesave = true
    
    if keystate_S then
        if keystate_allow_filesave then
            open "hypers.txt" for output as #1
            forloop_hparams( hypers )
            print #1, p->name, p->f
            next
            close #1
            windowtitle "hypers.txt saved.": sleep 1500
            keystate_allow_filesave = false
            windowtitle ""
        endif
    endif
    
    sleep 1
    kstr = inkey
loop

ImageDestroy(source_image)
ImageDestroy(final_image)

