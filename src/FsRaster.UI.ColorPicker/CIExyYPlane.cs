using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public class CIExyYPlane
        : Image, IColorPlane<ColorxyYFull>
    {
        private static readonly Figures.RenderPrimitive[] Horseshoe = FsRaster.CIExyYPlane.renderxyHorseshoe(ColorxyY.MaxValue, ColorxyY.MaxValue);

        public static readonly DependencyProperty YProperty =
            DependencyProperty.Register("Y", typeof(double), typeof(CIExyYPlane), new PropertyMetadata((double)0.0, OnYChanged));

        private readonly WriteableBitmap xyPlane = BitmapFactory.New(ColorxyY.MaxValue, ColorxyY.MaxValue);

        private bool shouldRegeneratexyPlane = false;
        private bool isLoaded = false;

        public double Y
        {
            get { return (double)GetValue(YProperty); }
            set { SetValue(YProperty, value); }
        }

        public ColorxyYFull Project(Point pt)
        {
            var x = pt.X / this.RenderSize.Width;
            var y = 1.0 - pt.Y / this.RenderSize.Height;
            return new ColorxyYFull(x, y, this.Y);
        }

        public Point Project(ColorxyYFull color)
        {
            var x = color.x * this.RenderSize.Width;
            var y = (1.0 - color.y) * this.RenderSize.Height;
            return new Point(x, y);
        }

        public ColorxyYFull Coerce(ColorxyYFull color, ColorxyYFull currentColor)
        {
            var x = Math.Round(color.x * ColorxyY.MaxValue);
            var y = Math.Round((1.0 - color.y) * ColorxyY.MaxValue);
            var pix = xyPlane.GetPixeli((int)x, (int)y);
            if (pix == 0)
            {
                return currentColor;
            }
            return Colors.Clamp(color);
        }

        public CIExyYPlane()
        {
            this.shouldRegeneratexyPlane = true;
            this.Source = xyPlane;

            this.Loaded += (s, e) =>
            {
                lock (this)
                {
                    this.isLoaded = true;
                    this.GeneratexyPlane();
                }
            };
            this.Unloaded += (s, e) => { lock (this) { this.isLoaded = false; } };

        }

        private static void OnYChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (CIExyYPlane)dp;
            lock (obj)
            {
                obj.shouldRegeneratexyPlane = true;
                if (obj.isLoaded)
                {
                    obj.GeneratexyPlane();
                }
            }
        }

        private unsafe void GeneratexyPlane()
        {
            lock (this)
            {
                if (this.shouldRegeneratexyPlane)
                {
                    using (var ctx = xyPlane.GetBitmapContext(ReadWriteMode.ReadWrite))
                    {
                        ctx.Clear();
                        var pixels = (uint*)ctx.Pixels;
                        foreach (var line in Horseshoe.Cast<Figures.RenderPrimitive.PrimLine>())
                        {
                            int x1 = line.Item.Item1;
                            int y = line.Item.Item2;
                            int x2 = line.Item.Item3;
                            int baseIndex = (ColorxyY.MaxValue - y) * ColorxyY.MaxValue;
                            for (int x = x1; x <= x2; x++)
                            {
                                var xyy = new ColorxyY(x, y, 0);
                                var xyyFull = Colors.ToxyYFull(xyy);
                                xyyFull = new ColorxyYFull(xyyFull.x, xyyFull.y, this.Y);
                                var xyz = Colors.ToXYZ(xyyFull);
                                var rgbFull = Colors.ToRGB(xyz);
                                var rgb = Colors.ToRGBFromXYZ(rgbFull);

                                pixels[baseIndex + x] = Colors.GetBytes(rgb);
                            }
                        }
                    }
                    this.shouldRegeneratexyPlane = false;
                }
            }
        }
    }
}
