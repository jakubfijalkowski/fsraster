using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public partial class HSVPicker
        : ColorPickerBase<ColorHSVFull>
    {
        public HSVPicker()
            : base(new ColorHSVFull(0, 1, 1))
        {
            InitializeComponent();
        }

        protected override IColorPlane<ColorHSVFull> ColorPlane
        {
            get
            {
                return this.hsRect;
            }
        }

        protected override FrameworkElement Pointer
        {
            get
            {
                return this.pointer;
            }
        }

        protected override ColorHSVFull UpdateColor()
        {
            var hue = this.hValue.Value.GetValueOrDefault(0);
            var saturation = this.sValue.Value.GetValueOrDefault(0);
            var value = this.vValue.Value.GetValueOrDefault(0);

            return new ColorHSVFull(hue, saturation, value);
        }

        protected override void UpdateControls()
        {
            this.hValue.Value = this.SelectedColor.Hue;
            this.sValue.Value = this.SelectedColor.Saturation;
            this.vValue.Value = this.SelectedColor.Value;
        }
    }
}
