using System;
using System.Windows.Media;

namespace FsRaster.UI.ColorPicker
{
    public struct ColorRGB
    {
        public const int MaxValue = 255;

        /// <summary>
        /// 0 - 255
        /// </summary>
        public readonly int R;

        /// <summary>
        /// 0 - 255
        /// </summary>
        public readonly int G;

        /// <summary>
        /// 0 - 255
        /// </summary>
        public readonly int B;

        public ColorRGB(int r, int g, int b)
        {
            this.R = r;
            this.G = g;
            this.B = b;
        }

        public bool Equals(ColorRGB rgb)
        {
            return this.R == rgb.R && this.G == rgb.G && this.B == rgb.B;
        }
    }

    public struct ColorRGBFull
    {
        public const double MaxValue = 1.0;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double R;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double G;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double B;

        public ColorRGBFull(double r, double g, double b)
        {
            this.R = r;
            this.G = g;
            this.B = b;
        }
    }

    public struct ColorHSV
    {
        public const int MaxHueValue = 360;
        public const int MaxValue = 150;

        /// <summary>
        /// 0 - 360
        /// </summary>
        public readonly int Hue;

        /// <summary>
        /// 0 - 150
        /// </summary>
        public readonly int Saturation;

        /// <summary>
        /// 0 - 150
        /// </summary>
        public readonly int Value;

        public ColorHSV(int hue, int saturation, int value)
        {
            this.Hue = hue;
            this.Saturation = saturation;
            this.Value = value;
        }
    }

    public struct ColorHSVFull
    {
        public const double MaxHueValue = 360.0;
        public const double MaxValue = 1.0;

        /// <summary>
        /// 0 - 360.0
        /// </summary>
        public readonly double Hue;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double Saturation;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double Value;

        public ColorHSVFull(double hue, double saturation, double value)
        {
            this.Hue = hue;
            this.Saturation = saturation;
            this.Value = value;
        }
    }

    public struct ColorXYZ
    {
        public const int MaxValue = 300;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int X;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int Y;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int Z;

        public ColorXYZ(int x, int y, int z)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
        }
    }

    public struct ColorXYZFull
    {
        public const double MaxValue = 2.0;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double X;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double Y;

        /// <summary>
        /// 0 - 1.0
        /// </summary>
        public readonly double Z;

        public ColorXYZFull(double x, double y, double z)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
        }
    }

    public struct ColorxyY
    {
        public const int MaxValue = 1000;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int x;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int y;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly int Y;

        public ColorxyY(int x, int y, int Y)
        {
            this.x = x;
            this.y = y;
            this.Y = Y;
        }
    }

    public struct ColorxyYFull
    {
        public const double MaxValue = 1.0;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly double x;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly double y;

        /// <summary>
        /// 0 - MaxValue
        /// </summary>
        public readonly double Y;

        public ColorxyYFull(double x, double y, double Y)
        {
            this.x = x;
            this.y = y;
            this.Y = Y;
        }
    }

    public static class Colors
    {
        const double Gamma = 1 / 2.4;
        const double InvGamma = 2.4;
        const double Transition = 0.003;
        const double Slope = 12.92;
        const double Offset = 0.055;
        const double Epsilon = 0.0001;

        private static readonly double[,] XYZToRGB = new double[3, 3]
        {
            {  3.2404542, -1.5371385, -0.4985314 },
            { -0.9692660,  1.8760108,  0.0415560 },
            {  0.0556434, -0.2040259,  1.0572252 }
        };

        private static readonly double[,] RGBToXYZ = new double[3, 3]
        {
            { 0.4124564, 0.3575761, 0.1804375 },
            { 0.2126729, 0.7151522, 0.0721750 },
            { 0.0193339, 0.1191920, 0.9503041 }
        };

        public static ColorRGB ToRGB(ColorRGBFull rgb)
        {
            var r = Math.Floor(Clamp(rgb.R) * ColorRGB.MaxValue);
            var g = Math.Floor(Clamp(rgb.G) * ColorRGB.MaxValue);
            var b = Math.Floor(Clamp(rgb.B) * ColorRGB.MaxValue);
            return new ColorRGB((int)r, (int)g, (int)b);
        }

        public static ColorHSV ToHSV(ColorHSVFull hsv)
        {
            var hue = Math.Round(hsv.Hue);
            var saturation = Math.Round(Clamp(hsv.Saturation) * ColorHSVFull.MaxValue);
            var value = Math.Round(Clamp(hsv.Value) * ColorHSVFull.MaxValue);
            return new ColorHSV((int)hue, (int)saturation, (int)value);
        }

        public static ColorXYZ ToXYZ(ColorXYZFull xyz)
        {
            var x = (int)Math.Round(Clamp(xyz.X) * ColorXYZ.MaxValue);
            var y = (int)Math.Round(Clamp(xyz.Y) * ColorXYZ.MaxValue);
            var z = (int)Math.Round(Clamp(xyz.Z) * ColorXYZ.MaxValue);
            return new ColorXYZ(x, y, z);
        }

        public static ColorRGBFull ToRGBFull(ColorRGB rgb)
        {
            var r = (double)rgb.R / ColorRGB.MaxValue;
            var g = (double)rgb.G / ColorRGB.MaxValue;
            var b = (double)rgb.B / ColorRGB.MaxValue;
            return new ColorRGBFull(r, g, b);
        }

        public static ColorHSVFull ToHSVFull(ColorHSV hsv)
        {
            var hue = hsv.Hue;
            var saturation = (double)hsv.Saturation / ColorHSV.MaxValue;
            var value = (double)hsv.Value / ColorHSV.MaxValue;
            return new ColorHSVFull(hue, saturation, value);
        }

        public static ColorXYZFull ToXYZFull(ColorXYZ xyz)
        {
            var x = (double)xyz.X / ColorXYZ.MaxValue * ColorXYZFull.MaxValue;
            var y = (double)xyz.Y / ColorXYZ.MaxValue * ColorXYZFull.MaxValue;
            var z = (double)xyz.Z / ColorXYZ.MaxValue * ColorXYZFull.MaxValue;
            return new ColorXYZFull(x, y, z);
        }

        public static ColorxyYFull ToxyYFull(ColorxyY xyy)
        {
            var x = (double)xyy.x / ColorxyY.MaxValue;
            var y = (double)xyy.y / ColorxyY.MaxValue;
            var Y = (double)xyy.Y / ColorxyY.MaxValue;
            return new ColorxyYFull(x, y, Y);
        }

        public static ColorRGBFull ToRGB(ColorHSVFull hsv)
        {
            double r = 0.0, g = 0.0, b = 0.0;

            if (hsv.Saturation == 0.0)
            {
                r = g = b = hsv.Value;
            }
            else
            {
                double h = (hsv.Hue == 360 ? 0 : hsv.Hue) / 60;

                int i = (int)h;
                double f = h - i,
                    p = hsv.Value * (1.0 - hsv.Saturation),
                    q = hsv.Value * (1.0 - hsv.Saturation * f),
                    t = hsv.Value * (1.0 - hsv.Saturation * (1.0 - f));
                switch (i)
                {
                    case 0:
                        r = hsv.Value;
                        g = t;
                        b = p;
                        break;

                    case 1:
                        r = q;
                        g = hsv.Value;
                        b = p;
                        break;

                    case 2:
                        r = p;
                        g = hsv.Value;
                        b = t;
                        break;

                    case 3:
                        r = p;
                        g = q;
                        b = hsv.Value;
                        break;

                    case 4:
                        r = t;
                        g = p;
                        b = hsv.Value;
                        break;

                    default:
                        r = hsv.Value;
                        g = p;
                        b = q;
                        break;
                }
            }
            return new ColorRGBFull(r, g, b);
        }

        public static ColorRGBFull ToRGB(ColorXYZFull xyz)
        {
            return Multiply(XYZToRGB, xyz.X, xyz.Y, xyz.Z);
        }

        public static ColorRGB ToRGBFromXYZ(ColorRGBFull rgb)
        {
            var r = Clamp(rgb.R);
            var g = Clamp(rgb.G);
            var b = Clamp(rgb.B);
            var max = Math.Max(r, Math.Max(g, b));
            r = GammaCorrect(r);
            g = GammaCorrect(g);
            b = GammaCorrect(b);
            r *= ColorRGB.MaxValue;
            g *= ColorRGB.MaxValue;
            b *= ColorRGB.MaxValue;
            return new ColorRGB((int)Math.Round(r), (int)Math.Round(g), (int)Math.Round(b));
        }

        public static ColorHSVFull ToHSV(ColorRGBFull rgb)
        {
            var min = Math.Min(rgb.R, Math.Min(rgb.G, rgb.B));
            var max = Math.Max(rgb.R, Math.Max(rgb.G, rgb.B));
            if (min == max)
            {
                return new ColorHSVFull(0, 0, min);
            }
            else
            {
                var f =
                    rgb.R == min ? rgb.G - rgb.B :
                    rgb.G == min ? rgb.B - rgb.R :
                                   rgb.R - rgb.G;
                var i =
                    rgb.R == min ? 3 :
                    rgb.G == min ? 5 :
                                   1;

                var hue = (i - f / (max - min)) * 60.0 % 360.0;
                var saturation = (max - min) / max;
                var value = max;
                return new ColorHSVFull(hue, saturation, value);
            }
        }

        public static ColorHSVFull ToHSVFromXYZ(ColorRGBFull rgb)
        {
            var r = Clamp(rgb.R);
            var g = Clamp(rgb.G);
            var b = Clamp(rgb.B);
            var max = Math.Max(r, Math.Max(g, b));
            r = GammaCorrect(r);
            g = GammaCorrect(g);
            b = GammaCorrect(b);
            return ToHSV(new ColorRGBFull(r, g, b));
        }

        public static ColorXYZFull ToXYZ(ColorRGBFull rgb)
        {
            var r = InverseGammaCorrection(rgb.R);
            var g = InverseGammaCorrection(rgb.G);
            var b = InverseGammaCorrection(rgb.B);
            var xyz = Multiply(RGBToXYZ, r, g, b);
            return new ColorXYZFull(xyz.R, xyz.G, xyz.B);
        }

        public static ColorXYZFull ToXYZ(ColorxyYFull xyy)
        {
            if (Math.Abs(xyy.y) < Epsilon)
            {
                return new ColorXYZFull(0, 0, 0);
            }
            double Yy = xyy.Y / xyy.y;
            double X = xyy.x * Yy;
            double Z = (1 - xyy.x - xyy.y) * Yy;
            return new ColorXYZFull(X, xyy.Y, Z);
        }

        public static ColorxyYFull ToxyY(ColorXYZFull xyz)
        {
            double sum = xyz.X + xyz.Y + xyz.Z;
            if (Math.Abs(sum) < Epsilon)
            {
                return new ColorxyYFull(0.3, 0.3, 1.0);
            }
            double x = xyz.X / sum;
            double y = xyz.Y / sum;
            return new ColorxyYFull(x, y, xyz.Y);
        }

        public static double Clamp(double v)
        {
            return Math.Max(0.0, Math.Min(1.0, v));
        }

        public static int Clamp(int v)
        {
            return Math.Max(0, Math.Min(255, v));
        }

        public static ColorRGB Clamp(ColorRGB color)
        {
            var r = Clamp(color.R);
            var g = Clamp(color.G);
            var b = Clamp(color.B);
            return new ColorRGB(r, g, b);
        }

        public static ColorRGBFull Clamp(ColorRGBFull rgb)
        {
            var r = Clamp(rgb.R);
            var g = Clamp(rgb.G);
            var b = Clamp(rgb.B);
            return new ColorRGBFull(r, g, b);
        }

        public static ColorHSVFull Clamp(ColorHSVFull hsv)
        {
            var hue = Math.Min(ColorHSVFull.MaxHueValue, Math.Max(0.0, hsv.Hue));
            var value = Clamp(hsv.Value);
            var saturation = Math.Min(Clamp(hsv.Saturation), value);
            return new ColorHSVFull(hue, saturation, value);
        }

        public static ColorXYZFull Clamp(ColorXYZFull xyz)
        {
            return xyz;
        }

        public static ColorxyYFull Clamp(ColorxyYFull xyy)
        {
            var x = Clamp(xyy.x);
            var y = Clamp(xyy.y);
            var Y = Clamp(xyy.Y);
            return new ColorxyYFull(x, y, Y);
        }

        public static uint GetBytes(uint r, uint g, uint b)
        {
            return 0xFF000000 | r << 16 | g << 8 | b;
        }

        public static uint GetBytes(ColorRGB rgb)
        {
            return 0xFF000000 | (uint)rgb.R << 16 | (uint)rgb.G << 8 | (uint)rgb.B;
        }

        public static uint GetBytes(ColorRGBFull rgb)
        {
            return GetBytes(ToRGB(rgb));
        }

        public static Color ToUIColor(ColorRGB newColor)
        {
            return Color.FromRgb((byte)newColor.R, (byte)newColor.G, (byte)newColor.B);
        }

        public static ColorRGB ToRGB(Color selectedColor)
        {
            return new ColorRGB(selectedColor.R, selectedColor.G, selectedColor.B);
        }

        private static ColorRGBFull Multiply(double[,] matrix, double a, double b, double c)
        {
            double x = matrix[0, 0] * a + matrix[0, 1] * b + matrix[0, 2] * c;
            double y = matrix[1, 0] * a + matrix[1, 1] * b + matrix[1, 2] * c;
            double z = matrix[2, 0] * a + matrix[2, 1] * b + matrix[2, 2] * c;
            return new ColorRGBFull(x, y, z);
        }

        private static double GammaCorrect(double value)
        {
            if (1 >= value && value >= Transition)
            {
                return (1 + Offset) * Math.Pow(value, Gamma) - Offset;
            }
            return Slope * value;
        }

        private static double InverseGammaCorrection(double value)
        {
            return Math.Pow((value + Offset) / (1 + Offset), InvGamma);
        }
    }

    public static class ColorsSafe
    {
        const double Epsilon = 0.0001;

        public static Tuple<ColorRGB, string> ToRGBFromHSV(ColorRGBFull rgb)
        {
            var result = Colors.ToRGB(rgb);
            return SafeConvert(rgb, result);
        }

        public static Tuple<ColorRGB, string> ToRGBFromXYZ(ColorRGBFull rgb)
        {
            var result = Colors.ToRGBFromXYZ(rgb);
            return SafeConvert(rgb, result);
        }

        public static Tuple<ColorHSVFull, string> ToHSVFromXYZ(ColorRGBFull rgb)
        {
            var result = Colors.ToHSVFromXYZ(rgb);
            if (rgb.R < 0.0 || rgb.R > 1.0 ||
                rgb.G < 0.0 || rgb.G > 1.0 ||
                rgb.B < 0.0 || rgb.B > 1.0)
            {
                return Tuple.Create(result, $"Cannot convert colors. Approximated by ~({result.Hue:##0.000}, {result.Saturation:0.000}, {result.Value:0.000})");
            }
            return Tuple.Create(result, string.Empty);
        }

        private static Tuple<ColorRGB, string> SafeConvert(ColorRGBFull rgb, ColorRGB result)
        {
            if (rgb.R < 0.0 || rgb.R > 1.0 ||
                rgb.G < 0.0 || rgb.G > 1.0 ||
                rgb.B < 0.0 || rgb.B > 1.0)
            {
                return Tuple.Create(result, $"Cannot convert colors. Approximated by ({result.R}, {result.G}, {result.B})");
            }
            return Tuple.Create(result, string.Empty);
        }
    }
}
