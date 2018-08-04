object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'RayMarching'
  ClientHeight = 557
  ClientWidth = 1126
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 449
    Top = 41
    Height = 427
    ExplicitLeft = 520
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  inline GLPanel1: TGLPanel
    Left = 452
    Top = 41
    Width = 674
    Height = 427
    Align = alClient
    TabOrder = 0
    OnMouseMove = GLPanel1MouseMove
    OnResize = GLPanel1Resize
    ExplicitLeft = 452
    ExplicitTop = 41
    ExplicitWidth = 674
    ExplicitHeight = 427
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 449
    Height = 427
    Align = alLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '// https://www.shadertoy.com/view/ldVGRV'
      'const int renderDepth = 400;'
      'const bool showRenderDepth = false;'
      ''
      'const vec3 background = vec3(0.2, 0.2, 0.8);'
      'const vec3 white = vec3(0.8, 0.8, 0.8);'
      'const vec3 black = vec3(0, 0, 0);'
      'const vec3 red = vec3(0.8, 0.2, 0.2);'
      'const vec3 green = vec3(0.2, 0.8, 0.2);'
      'const vec3 lightPos = vec3(0, 10, 10);'
      ''
      'const float a = 1.0;'
      'const float b = 3.0;'
      'vec3 forces[3];'
      ''
      ''
      'vec3 getRayDir(vec3 camDir, vec2 fragCoord) {'
      '  vec3 yAxis = vec3(0, 1, 0);'
      '  vec3 xAxis = normalize(cross(camDir, yAxis));'
      '  vec2 q = fragCoord / iResolution.xy;'
      '  vec2 p = 2.0 * q - 1.0;'
      '  p.x *= iResolution.x / iResolution.y;'
      '  return normalize(p.x * xAxis + p.y * yAxis + 5.0 * camDir);'
      '}'
      ''
      
        '// http://iquilezles.org/www/articles/distfunctions/distfunction' +
        's.htm'
      'float sdBox(vec3 p, vec3 b) {'
      '  vec3 d = abs(p) - b;'
      '  return min(max(d.x, max(d.y, d.z)), 0.0) + '
      'length(max(d, 0.0)) - 0.1;'
      '}'
      ''
      'float sdPlane(vec3 p, vec4 n) {'
      '  return dot(p,n.xyz) + n.w;'
      '}'
      ''
      'float getMetaball(vec3 p, vec3 v) {'
      '  float r = length(p - v);'
      '  if (r < b / 3.0) {'
      '    return a * (1.0 - 3.0 * r * r / b * b);'
      '  } else if (r < b) {'
      '    return (3.0 * a / 2.0) * (1.0 - r / b) * (1.0 - r / b);'
      '  } else {'
      '    return 0.0;'
      '  }'
      '}'
      ''
      'float sdImplicitSurface(vec3 p) {'
      '  float mb = 0.0;'
      '  float minDist = 10000.0;'
      '  for (int i = 0; i < 3; i++) {'
      '    mb += getMetaball(p, forces[i]);'
      '    minDist = min(minDist, length(p - forces[i]));'
      '  }'
      '  if (minDist > b) {'
      '    return max (minDist - b, b - 1.2679529);'
      '  } else if (mb == 0.0) {'
      
        '    return b - 1.2679529;  // 1.2679529 is the x-intercept of th' +
        'e metaball expression - 0.5.'
      '  } else {'
      '    return b - sqrt(6.0 * mb) - 1.2679529;'
      '  }'
      '}'
      ''
      'float getSdf(vec3 p) {'
      '  float f = sdImplicitSurface(p);'
      '  for (int i = 0; i < 6; i++) {'
      '    float t = float(i) + iGlobalTime;'
      '    f = min(f, sdBox('
      '        p - vec3('
      '            3.0 + 3.0 * cos(t * 3.141592 / 3.0),'
      '            0,'
      '            3.0 * sin(t * 3.141592 / 3.0)),'
      '        vec3(0.5)));'
      '  }'
      '  return f;'
      '}'
      ''
      'float getSdfWithPlane(vec3 p) {'
      '  return min(getSdf(p), sdPlane(p, vec4(0,1,0,1)));'
      '}'
      ''
      'float diffuse(vec3 point,vec3 normal) {'
      
        '  return clamp(dot(normal, normalize(lightPos - point)), 0.0, 1.' +
        '0);'
      '}'
      ''
      'float getShadow(vec3 pt) {'
      '  vec3 lightDir = normalize(lightPos - pt);'
      '  float kd = 1.0;'
      '  int step = 0;'
      '  float t = 0.1;'
      ''
      '  for (int step = 0; step < renderDepth; step++)'
      '{'
      '    float d = getSdf(pt + t * lightDir);'
      '    if (d < 0.001) {'
      '      kd = 0.0;'
      '    } else {'
      '      kd = min(kd, 16.0 * d / t);'
      '    }'
      '    t += d;'
      
        '    if (t > length(lightPos - pt) || step >= renderDepth || kd <' +
        ' 0.001) {'
      '      break;'
      '    }'
      '  }'
      '  return kd;'
      '}'
      ''
      'vec3 getGradient(vec3 pt) {'
      '  return vec3('
      
        '    getSdfWithPlane(vec3(pt.x + 0.0001, pt.y, pt.z)) - getSdfWit' +
        'hPlane(vec3(pt.x - 0.0001, pt.y, pt.z)),'
      
        '    getSdfWithPlane(vec3(pt.x, pt.y + 0.0001, pt.z)) - getSdfWit' +
        'hPlane(vec3(pt.x, pt.y -0.0001, pt.z)),'
      
        '    getSdfWithPlane(vec3(pt.x, pt.y, pt.z + 0.0001)) - getSdfWit' +
        'hPlane(vec3(pt.x, pt.y, pt.z - 0.0001)));'
      '}'
      ''
      'vec3 getDistanceColor(vec3 pt) {'
      '  float d = getSdf(pt);'
      '  vec3 color = mix(red, green, 0.5 + 0.5 * sin(d * 3.141592));'
      '  if (fract(d) < 0.05) {'
      '    color = mix(color, black, smoothstep(0.0, 0.05, fract(d)));'
      '  } else if (fract(d) < 0.1) {'
      '    color = mix(black, color, smoothstep(0.05, 0.1, fract(d)));'
      '  }'
      '  return color;'
      '}'
      ''
      'vec3 illuminate(vec3 pt) {'
      
        '  vec3 color = (abs(pt.y + 1.0) < 0.001) ? getDistanceColor(pt) ' +
        ': white;'
      '  vec3 gradient = getGradient(pt);'
      '  float diff = diffuse(pt.xyz, normalize(gradient));'
      '  return (0.25 + diff * getShadow(pt))  * color;'
      '}'
      ''
      'vec3 raymarch(vec3 rayorig, vec3 raydir) {'
      '  vec3 pos = rayorig;'
      '  float d = getSdfWithPlane(pos);'
      '  int work = 0;'
      ''
      '  for (int step = 0; step < renderDepth; step++) '
      '{'
      '    work++;'
      '    pos = pos + raydir * d;'
      '    d = getSdfWithPlane(pos);'
      '    if (abs(d) < 0.001) {'
      '      break;'
      '    }'
      '  }'
      ''
      '  return showRenderDepth'
      '    ? vec3(float(work) / float(renderDepth))'
      '    : (abs(d) < 0.001)'
      '      ? illuminate(pos)'
      '      : background;'
      '}'
      ''
      'void mainImage( out vec4 fragColor, in vec2 fragCoord ) {'
      '  forces[0] = vec3(-3, 0, 0);'
      
        '  forces[1] = vec3(3.0 * sin(iGlobalTime), 4.0 * abs(cos(iGlobal' +
        'Time)), 0.0);'
      '  forces[2] = vec3(3, 0, 0);'
      ''
      
        '  vec3 camPos = 20.0 * vec3(cos(iGlobalTime / 10.0), 0.5, sin(iG' +
        'lobalTime / 10.0));'
      '  vec3 camDir = normalize(-camPos);'
      
        '  fragColor = vec4(raymarch(camPos, getRayDir(camDir, fragCoord)' +
        '), 1.0);'
      '}')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnKeyDown = Memo1KeyDown
  end
  object Memo2: TMemo
    Left = 0
    Top = 468
    Width = 1126
    Height = 89
    Align = alBottom
    Lines.Strings = (
      'Memo2')
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1126
    Height = 41
    Align = alTop
    TabOrder = 3
    object Button1: TButton
      Left = 7
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Compile (F9)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 89
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Paste'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 216
  end
end
