' Flag Simulation v0.40 alpha
' Coded by UEZ build 2025-09-05

#include "crt/math.bi"
#include "fbgfx.bi"
Using FB

#define Min(a, b)    (IIf(a < b, a, b))
#define Max(a, b)    (IIf(a > b, a, b))

' Grid and physics constants
Const nX = 50, nY = Int(nX * 0.6666)   ' Grid points
Const Spacing = 7                      ' Distance between points
Const diagSpacing = Spacing * Sqr(2)   ' Diagonal spacing
Const Damping = 0.994                  ' Physics damping factor
Const Gravity = 0.175                  ' Gravity force
Const ConstraintIter = 2               ' Constraint iterations

' 2D Vector type
Type Vec2D
    x As Double
    y As Double
End Type

Dim Shared As Vec2D posi(nX, nY), oldPos(nX, nY)

' Perlin Noise implementation by Joshy aka D.J. Peters (R.I.P.)
Type REAL As Double
#define rAbs(x_)    IIf( (x_) < 0, -(x_), (x_) )
Const As REAL rPI = Acos(-1)
Const As REAL rDeg2Rad = rPI / 180

Type PERLINNOISE '...'
    Declare Constructor
    Declare Sub NoiseSeed(ByVal seed As Double)
    Declare Sub NoiseDetail(ByVal lod As Integer)
    Declare Sub NoiseDetail(ByVal lod As Integer, ByVal falloff As REAL)
    Declare Function Noise1D(ByVal x As REAL) As REAL
    Declare Function Noise2D(ByVal x As REAL, ByVal y As REAL) As REAL
    Declare Function Noise3D(ByVal x As REAL, ByVal y As REAL, ByVal z As REAL) As REAL
    
Private:
    Const As REAL    SINCOS_PRECISION = 0.5
    Const As Integer SINCOS_LENGTH    = (360 / SINCOS_PRECISION)
    Const As Integer PERLIN_YWRAPB    = 4
    Const As Integer PERLIN_YWRAP     = 1 Shl PERLIN_YWRAPB
    Const As Integer PERLIN_ZWRAPB    = 8
    Const As Integer PERLIN_ZWRAP     = 1 Shl PERLIN_ZWRAPB
    Const As Integer PERLIN_SIZE      = 4095
    Const As Integer PERLIN_TWOPI     = SINCOS_LENGTH
    Const As Integer PERLIN_PI        = PERLIN_TWOPI Shr 1
    
    As Integer perlin_octaves         = 4   ' Default to medium smooth
    As REAL    perlin_amp_falloff     = 0.5 ' 50% reduction per octave
    As REAL    perlin_cosTable(SINCOS_LENGTH-1)
    As REAL    perlin(PERLIN_SIZE)
    
    Declare Sub reInit
    Declare Function noise_fsc(ByVal i As REAL) As REAL
End Type

Constructor PERLINNOISE '...'
    For i As Integer = 0 To SINCOS_LENGTH - 1
        perlin_cosTable(i) = Cos(i * rDeg2Rad * SINCOS_PRECISION)
    Next
    reInit
End Constructor

Sub PERLINNOISE.reInit '...'
    For i As Integer = 0 To PERLIN_SIZE
        perlin(i) = Rnd()
    Next
End Sub

Function PERLINNOISE.noise_fsc(ByVal i As REAL) As REAL '...'
    Dim As Integer index = Int(i * PERLIN_PI)
    Return 0.5 * (1.0 - perlin_cosTable(index Mod SINCOS_LENGTH))
End Function

Sub PERLINNOISE.NoiseSeed(ByVal seed As Double) '...'
    Randomize(seed) : reInit
End Sub

Sub PERLINNOISE.NoiseDetail(ByVal lod As Integer) '...'
    If (lod > 0) Then perlin_octaves = lod
End Sub

Sub PERLINNOISE.NoiseDetail(ByVal lod As Integer, ByVal falloff As REAL) '...'
    If (lod > 0) Then perlin_octaves = lod
    If (falloff > 0) Then perlin_amp_falloff = falloff
End Sub

Function PERLINNOISE.Noise1D(ByVal x As REAL) As REAL '...'
    Return Noise3D(x, 0, 0)
End Function

Function PERLINNOISE.Noise2D(ByVal x As REAL, ByVal y As REAL) As REAL '...'
    Return Noise3D(x, y, 0)
End Function

Function PERLINNOISE.Noise3D(ByVal x As REAL, ByVal y As REAL, ByVal z As REAL) As REAL '...'
    x = rAbs(x) : y = rAbs(y) : z = rAbs(z)
    Dim As Integer xi = Int(x), yi = Int(y), zi = Int(z)
    Dim As REAL xf = x - xi, yf = y - yi, zf = z - zi
    Dim As REAL r, ampl = 0.5
    
    For i As Integer = 0 To perlin_octaves - 1
        Dim As Integer of = xi + (yi Shl PERLIN_YWRAPB) + (zi Shl PERLIN_ZWRAPB)
        Dim As REAL rxf = noise_fsc(xf)
        Dim As REAL ryf = noise_fsc(yf)
        Dim As REAL n1 = perlin(of And PERLIN_SIZE)
        n1 += rxf * (perlin((of + 1) And PERLIN_SIZE) - n1)
        Dim As REAL n2 = perlin((of + PERLIN_YWRAP) And PERLIN_SIZE)
        n2 += rxf * (perlin((of + PERLIN_YWRAP + 1) And PERLIN_SIZE) - n2)
        n1 += ryf * (n2 - n1)
        of += PERLIN_ZWRAP
        n2 = perlin(of And PERLIN_SIZE)
        n2 += rxf * (perlin((of + 1) And PERLIN_SIZE) - n2)
        Dim As REAL n3 = perlin((of + PERLIN_YWRAP) And PERLIN_SIZE)
        n3 += rxf * (perlin((of + PERLIN_YWRAP + 1) And PERLIN_SIZE) - n3)
        n2 += ryf * (n3 - n2)
        n1 += noise_fsc(zf) * (n2 - n1)
        r += n1 * ampl
        ampl *= perlin_amp_falloff
        xi Shl = 1: xf *= 2
        yi Shl = 1: yf *= 2
        zi Shl = 1: zf *= 2
        If (xf >= 1) Then xi += 1 : xf -= 1
        If (yf >= 1) Then yi += 1 : yf -= 1
        If (zf >= 1) Then zi += 1 : zf -= 1
    Next
    
    Return r
End Function

' Triangle drawing function with scanline filling
Sub DrawFilledTriangle(x1 As Long, y1 As Long, _
                       x2 As Long, y2 As Long, _
                       x3 As Long, y3 As Long, _
                       col As ULong)
    
    Dim As Long minY, maxY, y, x
    Dim As Long intersections, xIntersect(2)
    Dim As Single slope1, slope2, xStart, xEnd
    
    ' Determine Y range
    minY = y1
    If y2 < minY Then minY = y2
    If y3 < minY Then minY = y3
    
    maxY = y1
    If y2 > maxY Then maxY = y2
    If y3 > maxY Then maxY = y3
    
    ' For each scanline
    For y = minY To maxY
        intersections = 0
        
        ' Calculate intersections with three edges
        If ((y1 <= y And y < y2) Or (y2 <= y And y < y1)) And y1 <> y2 Then
            xIntersect(intersections) = x1 + (y - y1) * (x2 - x1) / (y2 - y1)
            intersections += 1
        End If
        
        If ((y2 <= y And y < y3) Or (y3 <= y And y < y2)) And y2 <> y3 Then
            xIntersect(intersections) = x2 + (y - y2) * (x3 - x2) / (y3 - y2)
            intersections += 1
        End If
        
        If ((y3 <= y And y < y1) Or (y1 <= y And y < y3)) And y3 <> y1 Then
            xIntersect(intersections) = x3 + (y - y3) * (x1 - x3) / (y1 - y3)
            intersections += 1
        End If
        
        ' Draw line between intersection points
        If intersections = 2 Then
            If xIntersect(0) > xIntersect(1) Then
                Swap xIntersect(0), xIntersect(1)
            End If
            Line (xIntersect(0), y)-(xIntersect(1), y), col
        End If
    Next
End Sub

' Apply distance constraint between two points
Sub ApplyConstraint(i1 As Long, j1 As Long, i2 As Long, j2 As Long, targetDist As Double) '...'
    Dim As Double dx = posi(i2, j2).x - posi(i1, j1).x
    Dim As Double dy = posi(i2, j2).y - posi(i1, j1).y
    Dim As Double dist = Sqr(dx * dx + dy * dy)
    If dist = 0 Then Exit Sub

    Dim As Double diff = (dist - targetDist) / dist
    
    ' If one point is fixed (column 0) -> apply full correction to movable point
    If i1 = 0 Then
        posi(i2, j2).x -= dx * diff
        posi(i2, j2).y -= dy * diff
    ElseIf i2 = 0 Then
        posi(i1, j1).x += dx * diff
        posi(i1, j1).y += dy * diff
    Else
        ' Both points movable -> split correction
        posi(i1, j1).x += dx * diff * 0.5
        posi(i1, j1).y += dy * diff * 0.5
        posi(i2, j2).x -= dx * diff * 0.5
        posi(i2, j2).y -= dy * diff * 0.5
    End If
End Sub

' Center window on screen
Sub CenterFBWin(iW As Long, iH As Long, iTBw As Long = 0, iTBh As Long = 0) '...'
    Dim As Long iScreenWidth, iScreenHeight
    ScreenControl GET_DESKTOP_SIZE, iScreenWidth, iScreenHeight
    ScreenControl SET_WINDOW_POS, (iScreenWidth - iW) \ 2 - iTBw, (iScreenHeight - iH) \ 2 - iTBh
End Sub

' ===== Flag in Wind Simulation =====
Randomize

' Initialize Perlin noise
Dim Shared pn As PERLINNOISE
pn.NoiseSeed(Timer)
pn.NoiseDetail(4, 0.5)

' FPS counter variables
Dim As UShort iFPS, cfps = 0
Dim As Double fTimer
Dim As Long i, j, iter

' Screen setup
Const w = 1200, h = 800
ScreenRes w, h, 32, 2, GFX_ALPHA_PRIMITIVES Or GFX_NO_SWITCH
ScreenSet 1, 0
Color &hFFFFFFFF, &hFF202020
Cls

' Initialize flag grid positions
Dim As Long px = w \ 2 - 15, py = 100

For i = 0 To nX
    For j = 0 To nY
        posi(i, j).x = px + i * Spacing
        posi(i, j).y = py + j * Spacing
        oldPos(i, j) = posi(i, j)
    Next
Next

' Simulation variables
Dim As Double t, wind, fx, fy
Dim As Double gust, windStrength, baseWind 
Dim As Double w1x, w1y, w2x, w2y, w3x, w3y
Dim As Double scale, vx, vy
Dim As Double x0, y0, x1, y1, x2, y2, x3, y3
Dim As Double dx, dy, bright, factor, n, flicker, dx1, dy1, dx2, dy2, cross
Dim As Long baseR, baseG, baseB
Dim As Long r, g, b
Dim As ULong col
Dim As Vec2D temp

' Main simulation loop
Do
    Cls
    
    ' Draw flag pole
    Line (px - 5, py - 30) - (px + 5, h), &hFF999999, BF
    Circle (px, py - 30), 10, &hFFDADBDD, , , , F
    
    ' 1) Verlet Integration - Physics simulation for each point
    For i = 1 To nX
        For j = 0 To nY
            temp = posi(i, j)
            
            ' Base movement = (current position - old position) * damping
            vx = (posi(i, j).x - oldPos(i, j).x) * Damping
            vy = (posi(i, j).y - oldPos(i, j).y) * Damping
            
            ' Wind force calculation per point
            
            ' Wind strength over time
            windStrength = 0.25 * pn.Noise1D(t * 0.002)
            
            ' Smooth transition (avoid harsh jumps)
            If windStrength < 0.1 Then windStrength = 0
            
            gust = 0.2 + pn.Noise1D(t * 0.05)
            
            ' Base wind fluctuates over time
            baseWind = windStrength  
            
            ' Large, slow wave
            w1x = (pn.Noise3D(i * 0.05, j * 0.08, t * 0.2) - 0.5) * 5
            w1y = (pn.Noise3D(i * 0.05, j * 0.08, t * 0.2 + 50) - 0.5) * 2
            
            ' Medium structure
            w2x = (pn.Noise3D(i * 0.12, j * 0.15, t * 0.6) - 0.5) * 6
            w2y = (pn.Noise3D(i * 0.12, j * 0.15, t * 0.6 + 100) - 0.5) * 3
            
            ' Fine fluttering
            w3x = (pn.Noise3D(i * 0.25, j * 0.25, t * 1.2) - 0.5) * 2
            w3y = (pn.Noise3D(i * 0.25, j * 0.25, t * 1.2 + 200) - 0.5) * 1.1
            
            ' Total wind = wind strength * (gusts + waves)
            scale = i / nX
            fx = scale * (baseWind + gust * (w1x + w2x + w3x))
            fy = scale * gust * (w1y + w2y + w3y + Gravity)
           
            ' Verlet integration: calculate new position
            posi(i, j).x += vx + fx
            posi(i, j).y += vy + fy + Gravity
            
            oldPos(i, j) = temp
        Next
    Next
    
    ' 2) Constraint system: correct distances between points
    For iter = 0 To ConstraintIter
        For i = 0 To nX
            For j = 0 To nY
                ' Horizontal connections
                If i < nX Then ApplyConstraint(i, j, i + 1, j, Spacing) 
                
                ' Vertical connections
                If j < nY Then ApplyConstraint(i, j, i, j + 1, Spacing) 
                
                ' Diagonal connections for stability
                If i < nX And j < nY Then
                    ApplyConstraint(i, j, i + 1, j + 1, diagSpacing)
                    ApplyConstraint(i + 1, j, i, j + 1, diagSpacing)                	
                End If
            Next
        Next
    Next
    
    ' 3) Fix first column to flag pole
    For j = 0 To nY
        posi(0, j).x = px
        posi(0, j).y = py + j * Spacing
    Next
    
    ' 4) Rendering: draw flag as triangles
    For i = 0 To nX - 1
        For j = 0 To nY - 1
            ' Corner points of the cell
            x0 = posi(i, j).x           : y0 = posi(i, j).y
            x1 = posi(i + 1, j).x       : y1 = posi(i + 1, j).y
            x2 = posi(i, j + 1).x       : y2 = posi(i, j + 1).y
            x3 = posi(i + 1, j + 1).x   : y3 = posi(i + 1, j + 1).y

            ' German flag colors: black-red-gold
            Select Case (j \ (nY / 3)) Mod 3
                Case 0: baseR = 24  : baseG = 24  : baseB = 24   ' Black
                Case 1: baseR = 200 : baseG = 0   : baseB = 0    ' Red
                Case 2: baseR = 240 : baseG = 200 : baseB = 40   ' Gold
            End Select
            
            ' Shading through wave deformation
            dx = ((x1 + x3) - (x0 + x2)) / 2
            dy = ((y2 + y3) - (y0 + y1)) / 2

            ' Cross product -> curvature, Abs() for symmetric brightness
            cross = dx * dy

            bright = 0.7 + 0.3 * (Abs(cross) / (Spacing * Spacing))

            ' Limit for realistic values
            If bright < 0.1 Then bright = 0.1
            If bright > 1.0 Then bright = 1.0

            ' Final color calculation
            r = baseR * bright
            g = baseG * bright
            b = baseB * bright

            col = RGB(r, g, b)

            ' Draw two triangles per quad
            DrawFilledTriangle(x0, y0, x1, y1, x2, y2, col)
            DrawFilledTriangle(x1, y1, x3, y3, x2, y2, col)
        Next
    Next

    t += 0.33333

    ' Display FPS counter
    Draw String(4, 4), iFPS & " fps", &hFFFFFFFF
    
    Flip

    ' FPS calculation
    If Timer - fTimer > 0.99 Then
        iFPS = cfps
        cfps = 0
        fTimer = Timer
    End If
    cfps += 1
    
    Sleep(10)
Loop Until Len(Inkey())
