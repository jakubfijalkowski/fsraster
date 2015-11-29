using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public sealed class HSRectangle
        : Image, IColorPlane<ColorHSVFull>
    {
        public static readonly DependencyProperty ValueProperty =
            DependencyProperty.Register("Value", typeof(double), typeof(HSRectangle), new PropertyMetadata((double)1.0, OnValueChanged));

        private WriteableBitmap hsPlane = BitmapFactory.New(ColorHSV.MaxValue * 2 + 1, ColorHSV.MaxValue * 2 + 1);

        public double Value
        {
            get { return (double)GetValue(ValueProperty); }
            set { SetValue(ValueProperty, value); }
        }

        private static void OnValueChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (HSRectangle)dp;
            obj.GenerateHSPlane();
        }

        public HSRectangle()
        {
            this.GenerateHSPlane();
            this.Source = this.hsPlane;
        }

        public Point Project(ColorHSVFull hsv)
        {
            var r = hsv.Saturation * ColorHSV.MaxValue;
            var theta = hsv.Hue / 180.0 * Math.PI;
            var y = ColorHSV.MaxValue + r * Math.Sin(theta);
            var x = ColorHSV.MaxValue + r * Math.Cos(theta);
            return new Point(x, y);
        }

        public ColorHSVFull Project(Point pt)
        {
            var dx = pt.X - ColorHSV.MaxValue;
            var dy = pt.Y - ColorHSV.MaxValue;
            double theta = Math.Atan2(dy, dx);
            if (theta < 0)
            {
                theta += Math.PI * 2;
            }
            var saturation = Math.Sqrt(dx * dx + dy * dy) / ColorHSV.MaxValue;
            saturation = Math.Min(saturation, this.Value);
            var hue = theta * 180.0 / Math.PI;

            return new ColorHSVFull(hue, saturation, this.Value);
        }

        public ColorHSVFull Coerce(ColorHSVFull color)
        {
            return Colors.Clamp(color);
        }

        private void GenerateHSPlane()
        {
            using (var ctx = this.hsPlane.GetBitmapContext(ReadWriteMode.ReadWrite))
            {
                ctx.Clear();
                unsafe
                {
                    var pixels = (uint*)ctx.Pixels;

                    var value = this.Value;

                    foreach (var pt in GenerateCircle((int)Math.Round(this.Value * ColorHSV.MaxValue)))
                    {
                        RenderHSLine(-pt.X, pt.X, pt.Y, value, pixels);
                        RenderHSLine(-pt.X, pt.X, -pt.Y, value, pixels);
                        RenderHSLine(-pt.Y, pt.Y, pt.X, value, pixels);
                        RenderHSLine(-pt.Y, pt.Y, -pt.X, value, pixels);
                    }
                }
            }
        }

        private unsafe static void RenderHSLine(int x1, int x2, int y, double value, uint* pixels)
        {
            const int ValueMaxValue = ColorHSV.MaxValue;
            x1 += ValueMaxValue;
            x2 += ValueMaxValue;
            y += ValueMaxValue;

            int dy = y - ValueMaxValue;

            int idx = y * (ValueMaxValue * 2 + 1);
            for (int x = x1; x <= x2; x++)
            {
                int dx = x - ValueMaxValue;
                double saturation = Math.Sqrt(dx * dx + dy * dy) / ValueMaxValue;
                double theta = Math.Atan2(dy, dx);
                if (theta < 0)
                {
                    theta += Math.PI * 2;
                }
                double hue = theta * 180.0 / Math.PI;
                uint color = Colors.GetBytes(Colors.ToRGB(new ColorHSVFull(hue, saturation, value)));
                pixels[idx + x] = color;
            }
        }

        private static IEnumerable<Point2D> GenerateCircle(int radius)
        {
            int deltaE = 3;
            int deltaSE = 5 - 2 * radius;
            int d = 1 - radius;

            int x = 0;
            int y = radius;

            yield return new Point2D(x, y);
            while (y > x)
            {
                if (d < 0)
                {
                    d += deltaE;
                    deltaE += 2;
                    deltaSE += 2;
                }
                else
                {
                    d += deltaSE;
                    deltaE += 2;
                    deltaSE += 4;
                    y--;
                }
                x++;
                yield return new Point2D(x, y);
            }
        }
    }
}
