using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace FsRaster.UI.ColorPicker
{
    public partial class AdvancedColorPicker : UserControl
    {
        public static readonly DependencyProperty SelectedColorProperty =
            DependencyProperty.Register("SelectedColor", typeof(Color), typeof(AdvancedColorPicker), new PropertyMetadata(System.Windows.Media.Colors.Black, OnSelectedColorChanged));
        public static readonly DependencyProperty AdvancedTitleProperty =
            DependencyProperty.Register("AdvancedTitle", typeof(string), typeof(AdvancedColorPicker), new PropertyMetadata(string.Empty));

        public static readonly RoutedEvent SelectedColorChangedEvent = EventManager.RegisterRoutedEvent(
            "SelectedColorChanged", RoutingStrategy.Direct, typeof(RoutedPropertyChangedEventHandler<Color>), typeof(AdvancedColorPicker));

        private MultiColorPickerWindow colorPickerWindow;

        public Color SelectedColor
        {
            get { return (Color)this.GetValue(SelectedColorProperty); }
            set { this.SetValue(SelectedColorProperty, value); }
        }

        public string AdvancedTitle
        {
            get { return (string)this.GetValue(AdvancedTitleProperty); }
            set { this.SetValue(AdvancedTitleProperty, value); }
        }

        public event RoutedPropertyChangedEventHandler<Color> SelectedColorChanged
        {
            add { this.AddHandler(SelectedColorChangedEvent, value); }
            remove { this.RemoveHandler(SelectedColorChangedEvent, value); }
        }

        public AdvancedColorPicker()
        {
            this.InitializeComponent();
        }

        private MultiColorPickerWindow CreateNewWindow()
        {
            var wnd = new MultiColorPickerWindow();
            wnd.Title = this.GetColorPickerTitle();
            wnd.SelectedColor = Colors.ToRGB(this.SelectedColor);
            wnd.Closing += OnColorPickerWindowClosing;
            wnd.PropertyChanged += OnColorPickerColorChanged;
            return wnd;
        }

        private static void OnSelectedColorChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var oldVal = (Color)e.OldValue;
            var newVal = (Color)e.NewValue;
            var obj = (AdvancedColorPicker)d;
            var args = new RoutedPropertyChangedEventArgs<Color>(oldVal, newVal, SelectedColorChangedEvent);
            obj.RaiseEvent(args);
        }

        private static void OnAdvancedTitleChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            lock (d)
            {
                var obj = (AdvancedColorPicker)d;
                if (obj.colorPickerWindow != null)
                {
                    obj.colorPickerWindow.Title = obj.GetColorPickerTitle();
                }
            }
        }

        private string GetColorPickerTitle()
        {
            if (string.IsNullOrEmpty(this.AdvancedTitle))
            {
                return "Color picker";
            }
            else
            {
                return $"Color picker - {AdvancedTitle}";
            }
        }

        private void OnColorPickerWindowClosing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            lock (this)
            {
                this.colorPickerWindow = null;
            }
        }

        private void OnColorPickerColorChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            lock (this)
            {
                if (this.colorPickerWindow != null && e.PropertyName == nameof(colorPickerWindow.SelectedColor))
                {
                    this.SelectedColor = Colors.ToUIColor(this.colorPickerWindow.SelectedColor);
                }
            }
        }

        private void OpenAdvancedWindow(object sender, RoutedEventArgs e)
        {
            lock (this)
            {
                if (this.colorPickerWindow == null)
                {
                    this.colorPickerWindow = this.CreateNewWindow();
                    this.colorPickerWindow.Show();
                }
                else
                {
                    this.colorPickerWindow.Activate();
                }
            }
        }

        private void OnInternalPickerColorChanged(object sender, RoutedPropertyChangedEventArgs<Color?> e)
        {
            lock (this)
            {
                if (this.colorPickerWindow != null)
                {
                    this.colorPickerWindow.SelectedColor = Colors.ToRGB(this.SelectedColor);
                }
            }
        }
    }
}
