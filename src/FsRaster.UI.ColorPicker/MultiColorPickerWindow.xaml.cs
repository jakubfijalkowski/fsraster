using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public partial class MultiColorPickerWindow
        : Window
    {
        public static readonly DependencyProperty SelectedColorProperty =
            DependencyProperty.Register("SelectedColor", typeof(ColorRGB), typeof(MultiColorPickerWindow), new PropertyMetadata(new ColorRGB()));

        public ColorRGB SelectedColor
        {
            get { return (ColorRGB)GetValue(SelectedColorProperty); }
            set { SetValue(SelectedColorProperty, value); }
        }

        public MultiColorPickerWindow()
        {
            InitializeComponent();

            this.SelectedColor = this.picker.SelectedColor;
            this.picker.PropertyChanged += OnPickerColorChanged;
        }

        private static void OnSelectedColorChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (MultiColorPickerWindow)dp;
            obj.OnSelectedColorChanged();
        }

        private void OnPickerColorChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            if (e.PropertyName == nameof(this.picker.SelectedColor))
            {
                this.SelectedColor = this.picker.SelectedColor;
            }
        }

        private void OnSelectedColorChanged()
        {
            this.picker.SelectedColor = this.SelectedColor;
        }
    }
}
