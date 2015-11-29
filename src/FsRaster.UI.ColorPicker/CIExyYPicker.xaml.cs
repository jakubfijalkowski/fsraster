using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public partial class CIExyYPicker
        : ColorPickerBase<ColorxyYFull>
    {
        public CIExyYPicker()
            : base(new ColorxyYFull(0.3, 0.3, 1))
        {
            InitializeComponent();
        }

        protected override IColorPlane<ColorxyYFull> ColorPlane
        {
            get
            {
                return this.xyRect;
            }
        }

        protected override FrameworkElement Pointer
        {
            get
            {
                return this.pointer;
            }
        }

        protected override ColorxyYFull UpdateColor()
        {
            var x = this.xValue.Value.GetValueOrDefault(0);
            var y = this.yValue.Value.GetValueOrDefault(0);
            var Y = this.YValue.Value.GetValueOrDefault(0);

            return new ColorxyYFull(x, y, Y);
        }

        protected override void UpdateControls()
        {
            this.xValue.Value = this.SelectedColor.x;
            this.yValue.Value = this.SelectedColor.y;
            this.YValue.Value = this.SelectedColor.Y;
        }
    }
}
