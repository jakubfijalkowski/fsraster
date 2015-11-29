using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace FsRaster.UI.ColorPicker
{
    public abstract class ColorPickerBase<TColor>
        : UserControl, INotifyPropertyChanged
    {
        private bool duringUpdate = false;
        private TColor selectedColor;

        public event PropertyChangedEventHandler PropertyChanged;

        public TColor SelectedColor
        {
            get { return this.selectedColor; }
            set
            {
                this.selectedColor = this.ColorPlane.Coerce(value);
                this.OnSelectedColorChanged();
                this.OnPropertyChanged();
            }
        }

        public ColorPickerBase(TColor def)
        {
            this.duringUpdate = true;
            this.selectedColor = def;

            this.SizeChanged += (s, e) => this.UpdatePointer();
            this.Loaded += (s, e) =>
            {
                this.UpdateControls();
                this.duringUpdate = false;
            };
        }

        protected abstract IColorPlane<TColor> ColorPlane { get; }
        protected abstract FrameworkElement Pointer { get; }

        protected abstract void UpdateControls();
        protected abstract TColor UpdateColor();

        protected void OnControlsColorChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            if (!this.duringUpdate)
            {
                this.SelectedColor = this.ColorPlane.Coerce(this.UpdateColor());
                this.UpdatePointer();
            }
        }

        protected void OnColorPlaneClicked(object sender, MouseEventArgs e)
        {
            var pos = e.GetPosition(this.ColorPlane);
            this.SelectedColor = this.ColorPlane.Project(pos);
            this.UpdateControls();
        }

        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            this.PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        private void OnSelectedColorChanged()
        {
            this.duringUpdate = true;
            this.UpdateControls();
            this.UpdatePointer();
            this.duringUpdate = false;
        }

        private void UpdatePointer()
        {
            var location = this.ColorPlane.Project(this.SelectedColor);
            var x = location.X - this.Pointer.Width / 2;
            var y = location.Y - this.Pointer.Height / 2;
            Canvas.SetLeft(this.Pointer, x);
            Canvas.SetTop(this.Pointer, y);
        }
    }
}
