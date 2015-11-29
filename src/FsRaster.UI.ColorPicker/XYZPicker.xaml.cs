using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public partial class XYZPicker
        : ColorPickerBase<ColorXYZFull>
    {
        public XYZPicker()
            : base(new ColorXYZFull(0, 0, 0))
        {
            InitializeComponent();
        }

        protected override IColorPlane<ColorXYZFull> ColorPlane
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

        protected override ColorXYZFull UpdateColor()
        {
            var x = this.xValue.Value.GetValueOrDefault(0);
            var y = this.yValue.Value.GetValueOrDefault(0);
            var z = this.zValue.Value.GetValueOrDefault(0);

            return new ColorXYZFull(x, y, z);
        }

        protected override void UpdateControls()
        {
            this.xValue.Value = this.SelectedColor.X;
            this.yValue.Value = this.SelectedColor.Y;
            this.zValue.Value = this.SelectedColor.Z;
        }
    }
}
