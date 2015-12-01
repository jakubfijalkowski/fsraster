using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public class CIEXYZPlane
        : Image, IColorPlane<ColorXYZFull>
    {
        public static readonly DependencyProperty YProperty =
            DependencyProperty.Register("Y", typeof(double), typeof(CIEXYZPlane), new PropertyMetadata((double)0.3, OnYChanged));

        private readonly WriteableBitmap xzPlane = BitmapFactory.New(ColorXYZ.MaxValue + 1, ColorXYZ.MaxValue + 1);

        private bool shouldReloadXZPlane = false;
        private bool isLoaded = false;

        public double Y
        {
            get { return (double)GetValue(YProperty); }
            set { SetValue(YProperty, value); }
        }

        private static void OnYChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (CIEXYZPlane)dp;
            lock (obj)
            {
                obj.shouldReloadXZPlane = true;
                if (obj.isLoaded)
                {
                    obj.GenerateXZPlane();
                }
            }
        }

        public CIEXYZPlane()
        {
            this.shouldReloadXZPlane = true;
            this.Source = this.xzPlane;

            this.Loaded += (s, e) =>
            {
                this.GenerateXZPlane();
                this.isLoaded = true;
            };
            this.Unloaded += (s, e) => this.isLoaded = false;
        }

        public Point Project(ColorXYZFull color)
        {
            var x = color.X * this.RenderSize.Width;
            var z = color.Z * this.RenderSize.Height;
            return new Point(x, z);
        }

        public ColorXYZFull Project(Point pt)
        {
            var x = pt.X / (this.RenderSize.Width - 1);
            var z = pt.Y / (this.RenderSize.Height - 1);
            return new ColorXYZFull(x, this.Y, z);
        }

        public ColorXYZFull Coerce(ColorXYZFull color, ColorXYZFull currentColor)
        {
            return Colors.Clamp(color);
        }

        private unsafe void GenerateXZPlane()
        {
            lock (this)
            {
                if (this.shouldReloadXZPlane)
                {
                    using (var ctx = this.xzPlane.GetBitmapContext(ReadWriteMode.ReadWrite))
                    {
                        var pixels = (uint*)ctx.Pixels;
                        for (int x = 0; x <= ColorXYZ.MaxValue; x++)
                        {
                            for (int z = 0; z <= ColorXYZ.MaxValue; z++)
                            {
                                var xyz = Colors.ToXYZFull(new ColorXYZ(x, 0, z));
                                var xyzrgb = Colors.ToRGB(new ColorXYZFull(xyz.X, this.Y, xyz.Z));
                                var rgb = Colors.ToRGBFromXYZ(xyzrgb);
                                pixels[z * (ColorXYZ.MaxValue + 1) + x] = Colors.GetBytes(rgb);
                            }
                        }
                    }
                    this.shouldReloadXZPlane = false;
                }
            }
        }
    }
}
