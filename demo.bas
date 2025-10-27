/' -- Lossy Image Compression with Randomized Circles - 2025 Oct 27  by dafhi

    Initial rollout by Gemini to get me inspired.
    
    one cool benefit from that version (July ish)
      fitness is an in-place before-and-after calc.
  
    before, i would write a circle to accum buf,
    error calc, then erase to prep next circle

      updates
    evolution: shapes_per_chan datapoints for curve approximation
    comments
    
  + -------------------------- +
  |    Language differences    |
  + -------------------------- +
  |  freebasic    |  c++       |
  + ------------- + ---------- +
  |  true = -1    |  true = 1  |
  |  0.99 =  1    |  0.99 = 0  |
  |  case insens  |            |
  +--------------- ----------- +

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

sub testpattern( imv as imvars )
    dim as long seed = 92, w = imv.w, h = imv.h, diagonal = sqr(w*w + h*h)
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

function clamp( in As double, hi As double = 1, lo As double = 0) As double '' 2025 Oct 21
    if in < lo then: in = lo: elseif in > hi then: in = hi : endif
    return in
End Function

function round(in as double, places as ubyte = 3) as string ' 2024 Aug 4
    dim as integer mul = 10 ^ places
    dim as string  ret = str( int(in * mul + .5) / mul )
    dim as long i_decimal = instr( ret, ".")
    if i_decimal < 1 then places = len(ret)
    return left( ret, i_decimal + places )
End Function

function sqr_safe( d as double ) as double
    return sgn(d) * sqr( abs(d))
end function


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
sub t_hyper_parameter.mutate( hi as single, lo as single)
    if rnd < .75 then '' 2025 Oct 21
        dim as single a = lo + rnd * (hi - lo)
        f = f + rnd * (a - f)
    else
        f = lo + rnd * (hi-lo)
    endif
end sub

    sub print_param( t as t_hyper_parameter, x as long = 0, y as long = 0, col as ulong = rgb(255,255,255) )
        draw string (x,y), t.name + " " + round(t.f, 3), col
    end sub


type hyper_parameters

    /'  code example : loop through all params

    forloop_hparams( @hypers ) ' <- included macro
      print "idx "; i, *p
    next
    '/

        #define forloop_hparams(q)  _     ' ready-to-use macro
    for p as t_hyper_parameter ptr = _
                        @(q).hdr_cbits to @(q).br_decay : _  ' < -- match First & Last hyperparams (defined below)
    dim as long i = p - @(q).hdr_cbits                       '' param index sometimes useful
    
    
    '' changes here may mess up loading an old hypers.txt
    
    as t_hyper_parameter hdr_cbits        = type( " hdr  cbits", 4 )
    as t_hyper_parameter shape_cbits      = type( "shape cbits", 4 )
    as t_hyper_parameter sim_cshapes      = type( "sim cshapes", 50 )
    as t_hyper_parameter sim_cs_decay     = type( "sim cs decay", .9 )
    as t_hyper_parameter cb_pen_shape     = type( "cb_pen_shape", .984 ) '' above 1 escapes local min
    as t_hyper_parameter f_cbits_decay    = type( "f_cbits decay", 0.625 )
    as t_hyper_parameter f_cbits          = type( "f_cbits   ", 3.97 )
    as t_hyper_parameter div_rad_ini      = type( "div rad ini", 65.6 )
    as t_hyper_parameter mag_rad_ini      = type( "mag rad ini", .372 )
    as t_hyper_parameter rad_decay        = type( "rad decay", .0173 )
    as t_hyper_parameter br               = type( "br", 42.7 )
    as t_hyper_parameter br_var           = type( "br var", 27.6 )
    as t_hyper_parameter br_var_decay     = type( "br var dcy", .78 )
    as t_hyper_parameter br_decay         = type( "br decay", .113 )

    as single score, MSE

end type

sub mutate( d as hyper_parameters ) '' reasonable ranges as i notice particular trends
    
    d.hdr_cbits.mutate .5, 5
    d.shape_cbits.mutate .5, 5
    
    d.sim_cshapes.mutate .5, 50
    d.sim_cs_decay.mutate .01, 5
    
    d.f_cbits.mutate 1., 6
    d.f_cbits_decay.mutate .76, .09

    d.div_rad_ini.mutate 4.25, 12
    d.mag_rad_ini.mutate .3, .8
    d.rad_decay.mutate .125, .009
    
    d.cb_pen_shape.mutate .7, 1.21 '' above 1 escapes local min but may become less than 1 as project evolves
    
    d.br.mutate 37, 59
    d.br_decay.mutate .17, .06
    d.br_var.mutate 13, 50
    d.br_var_decay.mutate 3., .001

end sub

sub print_hypers( h as hyper_parameters, x as long, y as long, col as ulong = rgb(0,0,255) )
    forloop_hparams( h )
        print_param *p, x, y + i*10, col
    next
end sub

Const BLUE_IDX  = 0 ' BGRA byte order for 32-bit screen/images in FreeBASIC GFX
Const GREEN_IDX = 1
Const RED_IDX   = 2
Const ALPHA_IDX = 3

    type ACCUM_TYPE as short
' ------------------------------


    Namespace hashed_circles_encoder
    
    #define rng  f_rng

dim as single           g_radius, hard_rad_max
dim as long             x0, x1, y0, y1, pos_neg, chan_idx, ix0, f_show, _dbg

Dim As ACCUM_TYPE       accumulator_buffer(any, any)
dim as long             component_average, hdr_cbits = 0, shape_cbits = 0
dim as hyper_parameters hypers
    
        function f_cbits( circl_index as long ) as double
            return hypers.f_cbits / (1 + circl_index * hypers.f_cbits_decay) '' experimental
        end function

type CircleShape
    as single x, y, rad, bright
end type

  sub _cliprect( imv as imvars, circl as CircleShape )
      x0 = max( circl.x - circl.rad, 0 )
      y0 = max( circl.y - circl.rad, 0 )
      x1 = min( circl.x + circl.rad, imv.w - 1 )
      y1 = min( circl.y + circl.rad, imv.h - 1 )
  end sub
    dim as ushort a_hdr( any, any ), a_seed( any, any ) ' ( shape_idx, channel )

Sub setup_channel_processing( imv as imvars, num_groups as ushort, channel_idx as long )
    redim accumulator_buffer(imv.w-1, imv.h-1)
    
      '' stack friendly seed array expansion
    dim as long u = ubound( a_hdr, 1 ), n = (num_groups + 1) * 1.35
    if num_groups > u then redim preserve a_hdr( n, 0 to 2 ), a_seed( n, 0 to 2 )
    
    hard_rad_max = (imv.w + imv.h) / hypers.div_rad_ini
    g_radius = hard_rad_max
    chan_idx = channel_idx
    hdr_cbits = hypers.hdr_cbits
    shape_cbits = hypers.shape_cbits
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
    dim as long _step = max( sqr(circl.rad * .43), 1 )
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

    dim as single     xy_a, xy_b, br_var, hard_rad_min = 0.77

  function f_radius( circl_index as long ) as double
      dim as single ratio = 1.07 + hypers.mag_rad_ini / (1 + circl_index * hypers.rad_decay)
      
      dim as long f = rng '' as Single or Long
      return clamp( g_radius * ( (2 - ratio)*f + ratio*(1-f) ), hard_rad_max, hard_rad_min )
  end function
    
  function f_brightness( circl_index as long ) as single
      return hypers.br / (1 + circl_index * hypers.br_decay)
  end function

  function f_brightness_variance( circl_index as long ) as single
      dim as single f = 1 / (1 + circl_index * hypers.br_var_decay)
      dim as single bright = hypers.br
      if rng < f then
      return bright * f
      else
      return bright * (1 - f) / 2
      endif
  end function

    sub rng_from_seeds( circle_index as long, seed as long, parent_rngstate as ulongint = 0 )
          using custom_rng
          a = seed * mulA
          a = (a xor circle_index) * mulA
          a = (a xor chan_idx) * mulA
          a = (a xor parent_rngstate) * mulB
    end sub
    
    function f_simgroup_cshapes( circl_index as long ) as single
        return hypers.sim_cshapes * (1 - 1 / (1 + circl_index * hypers.sim_cs_decay) )
    end function

        dim as ulongint     group_rng_state, g_grp_idx, cbits_total, modu
        dim as CircleShape  circ

    sub circle_props( imv as imvars, circle_idx as long, seed_circs as ushort)
        circle_idx += g_grp_idx
        rng_from_seeds circle_idx, seed_circs, group_rng_state
        circ.x = (imv.w-1) * (rng * 1.2 - .1) '' 2025 Oct 25 .. old: imv.w * (rng * 1.1 - .5)
        circ.y = (imv.h-1) * (rng * 1.2 - .1)
        modu = max( 1, f_simgroup_cshapes( circle_idx ) )
        if (circle_idx-1)mod modu = 0 then
              br_var  = f_brightness_variance( circle_idx )
              pos_neg = iif( rng < .5, -1, 1 )
            circ.bright = pos_neg * ( .5 + rng + f_brightness( circle_idx ) + rng * br_var )
            circ.rad = f_radius( circle_idx )
        endif
    end sub
    
        dim as long   cbits_circles, best_seed, n_groups

    sub group_derivates( hdr as ushort, grp_idx as long )
        rng_from_seeds( grp_idx, hdr, 0 )
        cbits_circles = rng * (shape_cbits + f_cbits(grp_idx) ) '' experimental f_cbits
        group_rng_state = custom_rng.a
        g_grp_idx = grp_idx
    end sub
    
    sub props_then_draw( im_src as imvars, seed_circs as ushort, decode_stage as long = true )
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
        dim as single score = f_group_score( im_src, hdr, idx ) / (1+hdr_cbits + cbits_circles) ^ hypers.cb_pen_shape
            If score > best_score Then
        best_score = score
        a_hdr(idx, chan_idx) = hdr
        a_seed(idx, chan_idx) = best_seed
        endif
        next
    End sub

    sub _decode__imv_and_mse( imv_des as imvars, imv_src as imvars )
        static as ubyte ptr des_pixels, src_pixels, row_des, row_src
        dim as longint delta_sum
        
        des_pixels = imv_des.pixels + chan_idx
        src_pixels = imv_src.pixels + chan_idx
        
            For y As Long = 0 To imv_des.h - 1
        row_des = des_pixels + y * imv_des.pitch
        row_src = src_pixels + y * imv_src.pitch
        
            For x As Long = 0 To imv_des.w - 1
        row_des[x*4] = clamp( accumulator_buffer(x,y) + component_average, 255 )
        dim as short d = row_des[x*4] - row_src[x*4]
        delta_sum += d * d
        Next
        Next y
        
        hypers.mse += delta_sum / imv_src.w / imv_src.h
    end sub
    
    sub accum_to_MSE( imv as imvars )
        dim as longint delta_sum
        For y As Long = 0 To imv.h - 1
            For x As Long = 0 To imv.w - 1
                dim as long _result = clamp( accumulator_buffer(x,y), component_average, component_average - 255 )
                delta_sum += _result * _result
            Next
        Next
        hypers.mse += delta_sum / imv.w / imv.h
    end sub
    
        Dim As imvars   imv_result, imv_small, imv_src

  Sub _decode( chan_idx As long)
      setup_channel_processing imv_result, n_groups, chan_idx
      For i As long = 1 to n_groups
          group_derivates a_hdr( i, chan_idx ), i
          props_then_draw imv_result, a_seed(i, chan_idx)
          g_radius = circ.rad
          cbits_total += hdr_cbits + cbits_circles
      Next
      _decode__imv_and_mse imv_result, imv_src
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
          props_then_draw imv, a_seed(i, chan_idx), decode_stage
          g_radius = circ.rad
          cbits_total += hdr_cbits + cbits_circles
      Next i
      
      accum_to_mse imv
      
  End Sub

  sub enc_dec__pre
      hypers.mse = 0
      cbits_total = 0
  end sub


  sub calculate_score
  
    ' -------------------------------------------------------------------------------
    '    div zero adjustments
    '
    '  MSE 0
    '  "no image loss" ?  Possibly, but cbits becomes pointless
    '
    '  MSE with base 1  cBits 0
    '  "no image loss" still a possibility.  allows edge case: infinite compression
    ' -------------------------------------------------------------------------------
    
    hypers.score = 1e6 / (hypers.MSE + 1) / (cbits_total + 0) ^ .5 '' exponent .5 works pretty good
    
  end sub


    sub choose_whether_to_downscale( r as single, byref p as imvars ptr, internal_image_diagonal as single )
        dim as single scale = min( internal_image_diagonal / r, 1 )
        if scale < 1 then
            fill_imvars imv_small, imagecreate( imv_src.w * scale, imv_src.h * scale )
            downscale imv_small, imv_src
            p = @imv_small
        endif
    end sub

sub encode( src as any ptr, num_groups as long, _cbits_circles as long )
    
    if imv_src.im <> src then fill_imvars imv_src, src
    
    dim as single r = sqr(imv_src.w^2+imv_src.h^2)
    if r < 2 then exit sub
    
    dim as imvars ptr p_imv = @imv_src
    choose_whether_to_downscale r, p_imv, 150 '' scaled diagonal
    cbits_circles = _cbits_circles
    enc_dec__pre
    n_groups = num_groups
    _encode *p_imv, red_IDX, n_groups
    _encode *p_imv, green_IDX, n_groups
    _encode *p_imv, blue_IDX, n_groups
    calculate_score
    
    f_show = true '' inform of an encode (reduces demo code)
    
end sub

sub update_seeds( src as any ptr, num_groups as long, circles_cbits as long )
    encode src, num_groups, circles_cbits
end sub

sub update_score( src as any ptr, num_groups as long, circles_cbits as long )
    encode src, num_groups, circles_cbits
end sub

sub decode( des as any ptr )
    if imv_result.im <> des then  fill_imvars(imv_result, des)
    enc_dec__pre
    _decode red_IDX
    _decode Green_IDX
    _decode Blue_IDX
end sub

End Namespace ' -- hashed_circls_encoder

' =============================================================================
'   MAIN PROGRAM
' =============================================================================

namespace evolver

  using hashed_circles_encoder
  
  dim as long bool_hypers_display, _dbg
  
  sub print_info( ix as long, iy as long, col as ulong )
      if _dbg then cls
      dim as long projected_future_header_size = 6, total_bytes = projected_future_header_size + (cbits_total + 7)shr 3
      dim as single ratio = imv_result.w * imv_result.h * 3 / total_bytes
      draw string (ix + 180, iy + 0), "bytes " + str( total_bytes ) + " .. ratio: " + round( ratio,1 ), col
      draw string (ix + 0, iy + 80), "shapes per channel " + str( n_groups ), col
      draw string (ix, iy + 45), "score (MSE, bits) " + round( hypers.score, 2),col
      draw string (ix, iy + 60), " MSE              " + round( hypers.MSE, 2),col
      iy += 45
      if bool_hypers_display then print_hypers hypers, ix + 235, iy, col
      if _dbg then windowtitle "DEBUG MODE PRINT_INFO" : sleep
  end sub

sub show( des as any ptr, ix as long=0, iy as long=0 )
    decode des
    Put (ix, iy), des, PSet
    print_info ix+1, iy+1, rgb(0,0,0)
    print_info ix, iy, rgb(255,255,255)
end sub

sub _shapecount_based_scoring( src as any ptr, a as single, b as single, cbits_custom as long )
    
    '' MSE across multiple "shapes per channel" datapoints, which results in curve fitting
    
    dim as hyper_parameters h0 = hypers
    
    dim as double score_sum_squared, score_squared_0 = h0.score ^ 2
        
        for sample_count as long = 1 to 4
    dim as single f = rnd
    dim as long c_shapes = a+f*(b-a)
    mutate hypers
    encode src, c_shapes, cbits_custom
    
    dim as double score_squared = hypers.score ^ 2
    score_sum_squared += score_squared
  
    dim as single avg_score_squared = score_sum_squared / sample_count

    '' low samples for speed & local min escape, but may trash good params
    '' early exit potential from outliers
    if sample_count >= 3 then
        if avg_score_squared < score_squared_0 * .6 then hypers = h0 : exit for
        if avg_score_squared > score_squared_0 * 1.5 then exit for
    endif
    
    if sample_count >= 2 then
        if avg_score_squared > score_squared_0 then
            h0 = hypers
        else
            hypers = h0
        endif
    endif

    locate 1,30
    ? avg_score_squared

    sleep 1
    next
    
end sub

sub _evolve( src as any ptr, cbits_custom as long )
    
    'update_score src, n_groups, cbits_custom
    dim as single b = n_groups, a = b ^ .85 '' shape count range
    _shapecount_based_scoring src, a, b, cbits_custom
    n_groups = b
    update_score src, n_groups, cbits_custom
    
end sub

sub evolve( src as any ptr, chan_cshapes as long = 100, u_loop as long = 0 )
    
    dim as long cbits_save = cbits_circles
    
    locate 1,1
    ? "scores from random params   "

    for i as long = 1 to u_loop
        _evolve src, cbits_save
    next
    update_seeds src, chan_cshapes, cbits_save
    locate 1,1 : ? "                  Finito !             "
end sub

End Namespace ' -- evolver

    randomize


Const IMG_W = 400
Const IMG_H = 350

dim as long shapes_per_chan = 14

ScreenRes IMG_W * 2 + 30, IMG_H + 20, 32

const black = rgb(0,0,0)

Dim As Any Ptr source_image = ImageCreate(IMG_W, IMG_H, 0, Black)

dim as imvars imv : fill_imvars imv, source_image
testpattern imv
Put (10, 10), source_image, PSet

using evolver
encode source_image, shapes_per_chan, cbits_circles

Dim As Any Ptr final_image = ImageCreate(IMG_W, IMG_H)
show final_image, IMG_W + 20, 10

dim as long keystate_A, keystate_0_A '' suppress key repeat knowing previous keystate (or fbgfx event class)
dim as long keystate_L, keystate_0_L

dim as long keystate_S, keystate_0_S, keystate_allow_fileaccess

dim as long keystate_CTRL '' keycode discovery:  #include "fbgfx.bi": using FB: print(SC_LSHIFT)
dim as long keystate_0_CTRL

  #define extchar chr(255) '' L Shift, Alt, etc.
dim as string kstr = ""
#if 1
    locate 4,1
    print "  Keys: " + chr(10) + chr(10) + _
    "  left right : circle count" + chr(10) + chr(10) + _
    "  a          : evolve hyperparameters" + chr(10) + _
    "  d          : toggle hypers display" + chr(10) + _
    "  Ctrl S     : save params (hypers.txt)" + chr(10) + _
    "  Ctrl L     : load params"
    
#endif
dim as double t = timer, dt, tp, t0 = t

chdir exepath

do

    tp = t : t = timer
    dt = t - tp
    
    keystate_0_A = keystate_A:    keystate_A = multikey(30)
    keystate_0_S = keystate_S:    keystate_S = multikey(31)
    keystate_0_L = keystate_L:    keystate_L = multikey(38)
    
    if keystate_A <> 0 and keystate_0_A = 0 then
        evolve source_image, shapes_per_chan, clamp( 9999 / (shapes_per_chan * (hdr_cbits + shape_cbits)^.7), 99, 1 )
    endif
    
    select case (kstr)
    case chr(27) '' esc
        exit do
    case "d", "D"
        bool_hypers_display = iif( bool_hypers_display, 0, 1 )
        f_show = true
    case extchar + chr(75) ' - LEFT -
        if shapes_per_chan > 1 then
        shapes_per_chan -= max(1, 0.1*shapes_per_chan )
        encode source_image, shapes_per_chan, cbits_circles
        endif
    case extchar + chr(77) ' - RIGHT -
        if shapes_per_chan < 1999 then
        shapes_per_chan += clamp( 0.25*shapes_per_chan, 150, 1)
        encode source_image, shapes_per_chan, cbits_circles
        endif
    end select
    
      if f_show then '' encode() assigns True to f_show
    show final_image, IMG_W + 20, 10
    f_show = false
    windowtitle ""
    endif
    
    '' prevents key repeated file ops
    keystate_0_CTRL = keystate_CTRL :  keystate_CTRL = multikey(29)
    if (keystate_0_CTRL = 0) and keystate_CTRL then keystate_allow_fileaccess = true
    
    if keystate_allow_fileaccess and keystate_CTRL then
    
        if keystate_S then
            open "hypers.txt" for output as #1
            forloop_hparams( hypers )
            print #1, p->name, round( p->f, 4 )
            next
            close #1
            windowtitle "hypers.txt saved.": sleep 1500
            keystate_allow_fileaccess = false
        endif
        
        if keystate_L then
            dim as string _name
            open "hypers.txt" for input as #1
                forloop_hparams( hypers )
                    input #1, _name
                    p->f = val(right(_name, len(_name) - instrrev(_name," ")))
                next
            close #1
            windowtitle "hypers.txt loaded.": sleep 1500
            keystate_allow_fileaccess = false
            update_seeds source_image, shapes_per_chan, cbits_circles
            f_show = true
        endif
    
    endif
    
    sleep 1
    kstr = inkey
loop

ImageDestroy(source_image)
ImageDestroy(final_image)
