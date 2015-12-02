using System;
using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public sealed class RGBPlane
        : ColorPlaneBase<ColorRGB>
    {
        public static readonly DependencyProperty BProperty =
            DependencyProperty.Register("B", typeof(byte), typeof(RGBPlane), new PropertyMetadata((byte)0, ColorPlaneBase<ColorRGB>.OnPropertyChanged));

        public byte B
        {
            get { return (byte)this.GetValue(BProperty); }
            set { this.SetValue(BProperty, value); }
        }

        public RGBPlane()
            : base(ColorRGB.MaxValue + 1, ColorRGB.MaxValue + 1)
        { }

        public override Point Project(ColorRGB color)
        {
            var x = color.R / 255.0 * this.RenderSize.Width;
            var y = (255 - color.G) / 255.0 * this.RenderSize.Height;
            return new Point(x, y);
        }

        public override ColorRGB Project(Point pt)
        {
            double xCoeff = pt.X / (this.RenderSize.Width - 1);
            double yCoeff = 1.0 - pt.Y / (this.RenderSize.Height - 1);
            return new ColorRGB((byte)Math.Round(xCoeff * 255.0), (byte)Math.Round(yCoeff * 255.0), this.B);
        }

        public override ColorRGB Coerce(ColorRGB color, ColorRGB currentColor)
        {
            return Colors.Clamp(color);
        }

        protected override unsafe void GeneratePlane(uint* pixels)
        {
            for (uint g = 0; g < 256; g++)
            {
                for (uint r = 0; r < 256; r++)
                {
                    long idx = (255 - g) * (ColorRGB.MaxValue + 1) + r;
                    pixels[idx] = Colors.GetBytes(r, g, this.B);
                }
            }
        }
    }
}