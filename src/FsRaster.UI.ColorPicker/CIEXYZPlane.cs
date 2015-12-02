using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public class CIEXYZPlane
        : ColorPlaneBase<ColorXYZFull>
    {
        public static readonly DependencyProperty YProperty =
            DependencyProperty.Register("Y", typeof(double), typeof(CIEXYZPlane), new PropertyMetadata((double)0.3, ColorPlaneBase<ColorXYZFull>.OnPropertyChanged));

        public double Y
        {
            get { return (double)GetValue(YProperty); }
            set { SetValue(YProperty, value); }
        }

        public CIEXYZPlane()
            : base(ColorXYZ.MaxValue + 1, ColorXYZ.MaxValue + 1)
        { }

        public override Point Project(ColorXYZFull color)
        {
            var x = color.X * this.RenderSize.Width;
            var z = color.Z * this.RenderSize.Height;
            return new Point(x, z);
        }

        public override ColorXYZFull Project(Point pt)
        {
            var x = pt.X / (this.RenderSize.Width - 1);
            var z = pt.Y / (this.RenderSize.Height - 1);
            return new ColorXYZFull(x, this.Y, z);
        }

        public override ColorXYZFull Coerce(ColorXYZFull color, ColorXYZFull currentColor)
        {
            return Colors.Clamp(color);
        }

        protected override unsafe void GeneratePlane(uint* pixels)
        {
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
    }
}
