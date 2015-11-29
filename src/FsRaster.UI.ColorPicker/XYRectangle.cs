using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public class XYRectangle
        : Image, IColorPlane<ColorXYZFull>
    {
        public static readonly DependencyProperty ZProperty =
            DependencyProperty.Register("Z", typeof(double), typeof(XYRectangle), new PropertyMetadata((double)0.3, OnYChanged));

        private readonly WriteableBitmap xyPlane = BitmapFactory.New(ColorXYZ.MaxValue + 1, ColorXYZ.MaxValue + 1);

        public double Z
        {
            get { return (double)GetValue(ZProperty); }
            set { SetValue(ZProperty, value); }
        }

        private static void OnYChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (XYRectangle)dp;
            obj.GenerateXYPlane();
        }

        public XYRectangle()
        {
            this.GenerateXYPlane();
            this.Source = this.xyPlane;
        }

        public Point Project(ColorXYZFull color)
        {
            var x = color.X * this.RenderSize.Width;
            var y = color.Y * this.RenderSize.Height;
            return new Point(x, y);
        }

        public ColorXYZFull Project(Point pt)
        {
            var x = pt.X / (this.RenderSize.Width - 1);
            var y = pt.Y / (this.RenderSize.Height - 1);
            return new ColorXYZFull(x, y, this.Z);
        }

        public ColorXYZFull Coerce(ColorXYZFull color)
        {
            return Colors.Clamp(color);
        }

        private void GenerateXYPlane()
        {
            using (var ctx = this.xyPlane.GetBitmapContext(ReadWriteMode.ReadWrite))
            {
                unsafe
                {
                    var pixels = (uint*)ctx.Pixels;
                    for (int x = 0; x <= ColorXYZ.MaxValue; x++)
                    {
                        for (int y = 0; y <= ColorXYZ.MaxValue; y++)
                        {
                            var xyz = Colors.ToXYZFull(new ColorXYZ(x, y, 0));
                            var xyzrgb = Colors.ToRGB(new ColorXYZFull(xyz.X, xyz.Y, this.Z));
                            var rgb = Colors.ToRGBFromXYZ(xyzrgb);
                            pixels[y * (ColorXYZ.MaxValue + 1) + x] = Colors.GetBytes(rgb);

                        }
                    }
                }
            }
        }
    }
}
