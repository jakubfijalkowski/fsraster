using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public partial class RGBPicker
        : ColorPickerBase<ColorRGB>
    {
        public RGBPicker()
            : base(new ColorRGB(127, 127, 255))
        {
            this.InitializeComponent();
        }

        protected override IColorPlane<ColorRGB> ColorPlane
        {
            get
            {
                return this.rgRect;
            }
        }

        protected override FrameworkElement Pointer
        {
            get
            {
                return this.pointer;
            }
        }

        protected override ColorRGB UpdateColor()
        {
            var r = this.rValue.Value.GetValueOrDefault(0);
            var g = this.gValue.Value.GetValueOrDefault(0);
            var b = this.bValue.Value.GetValueOrDefault(0);

            return new ColorRGB((byte)r, (byte)g, (byte)b);
        }

        protected override void UpdateControls()
        {
            this.rValue.Value = this.SelectedColor.R;
            this.gValue.Value = this.SelectedColor.G;
            this.bValue.Value = this.SelectedColor.B;
        }
    }
}
