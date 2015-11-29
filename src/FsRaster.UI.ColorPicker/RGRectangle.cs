using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public sealed class RGRectangle
        : Image, IColorPlane<ColorRGB>
    {
        public static readonly DependencyProperty BProperty =
            DependencyProperty.Register("B", typeof(byte), typeof(RGRectangle), new PropertyMetadata((byte)0, OnBChanged));

        private readonly WriteableBitmap rgbPlane = BitmapFactory.New(256, 256);

        public byte B
        {
            get { return (byte)this.GetValue(BProperty); }
            set { this.SetValue(BProperty, value); }
        }

        private static void OnBChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var obj = (RGRectangle)d;
            obj.GenerateRGBPlane();
        }

        public RGRectangle()
        {
            this.GenerateRGBPlane();
            this.Source = this.rgbPlane;
        }

        public Point Project(ColorRGB color)
        {
            var x = color.R / 255.0 * this.RenderSize.Width;
            var y = (255 - color.G) / 255.0 * this.RenderSize.Height;
            return new Point(x, y);
        }

        public ColorRGB Project(Point pt)
        {
            double xCoeff = pt.X / (this.RenderSize.Width - 1);
            double yCoeff = 1.0 - pt.Y / (this.RenderSize.Height - 1);
            return new ColorRGB((byte)Math.Round(xCoeff * 255.0), (byte)Math.Round(yCoeff * 255.0), this.B);
        }

        public ColorRGB Coerce(ColorRGB color)
        {
            return color;
        }

        private void GenerateRGBPlane()
        {
            using (var ctx = this.rgbPlane.GetBitmapContext(ReadWriteMode.ReadWrite))
            {
                unsafe
                {
                    var pixels = (uint*)ctx.Pixels;
                    for (uint g = 0; g < 256; g++)
                    {
                        for (uint r = 0; r < 256; r++)
                        {
                            long idx = (255 - g) * ctx.Height + r;
                            pixels[idx] = Colors.GetBytes(r, g, this.B);
                        }
                    }
                }
            }
        }
    }
}
