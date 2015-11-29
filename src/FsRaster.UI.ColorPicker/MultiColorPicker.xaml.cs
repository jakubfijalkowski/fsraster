using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Windows.Controls;

namespace FsRaster.UI.ColorPicker
{
    public partial class MultiColorPicker
        : UserControl, INotifyPropertyChanged
    {
        private ColorRGB selectedColor = new ColorRGB();
        private string conversionMessage = string.Empty;
        private bool duringTabChange = false;

        public event PropertyChangedEventHandler PropertyChanged;

        public ColorRGB SelectedColor
        {
            get { return this.selectedColor; }
            set
            {
                if (!this.selectedColor.Equals(value))
                {
                    this.selectedColor = value;
                    this.OnSelectedColorChanged();
                }
            }
        }

        public string ConversionMessage
        {
            get { return this.conversionMessage; }
            set
            {
                this.conversionMessage = value;
                this.OnPropertyChanged();
            }
        }

        public MultiColorPicker()
        {
            this.DataContext = this;
            InitializeComponent();

            this.OnSelectedColorChanged();
        }

        private void ResetMessage()
        {
            if (!this.duringTabChange)
            {
                this.ConversionMessage = string.Empty;
            }
        }

        protected void OnPropertyChanged([CallerMemberName] string propertyName = "")
        {
            this.PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        private void OnRGBColorChanged(object sender, PropertyChangedEventArgs e)
        {
            if (this.rgbTab.IsSelected)
            {
                this.UpdateSelectedColor(this.rgbPicker.SelectedColor);
                this.ResetMessage();
            }
        }

        private void OnHSVColorChanged(object sender, PropertyChangedEventArgs e)
        {
            if (this.hsvTab.IsSelected)
            {
                var rgb = Colors.ToRGB(this.hsvPicker.SelectedColor);
                this.UpdateSelectedColor(Colors.ToRGB(rgb));
                this.ResetMessage();
            }
        }

        private void OnXYZColorChanged(object sender, PropertyChangedEventArgs e)
        {
            if (this.xyzTab.IsSelected)
            {
                var xyzrgb = Colors.ToRGB(this.xyzPicker.SelectedColor);
                var rgb = Colors.ToRGBFromXYZ(xyzrgb);
                this.UpdateSelectedColor(rgb);
                this.ResetMessage();
            }
        }

        private void xyYPlane_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (this.xyYTab.IsSelected)
            {
                var xyz = Colors.ToXYZ(this.xyYPicker.SelectedColor);
                var xyzrgb = Colors.ToRGB(xyz);
                var rgb = Colors.ToRGBFromXYZ(xyzrgb);
                this.UpdateSelectedColor(rgb);
                this.ResetMessage();
            }
        }

        private void OnTabChanged(object sender, SelectionChangedEventArgs e)
        {
            if (e.RemovedItems.Count == 0 || e.AddedItems.Count == 0)
            {
                return;
            }
            this.duringTabChange = true;
            if (e.RemovedItems[0] == this.rgbTab)
            {
                if (e.AddedItems[0] == this.hsvTab)
                {
                    this.ConvertFromRGBToHSV();
                }
                else if (e.AddedItems[0] == this.xyzTab)
                {
                    this.ConvertFromRGBToXYZ();
                }
                else
                {
                    this.ConvertFromRGBToxyY();
                }
            }
            else if (e.RemovedItems[0] == this.hsvTab)
            {
                if (e.AddedItems[0] == this.rgbTab)
                {
                    this.ConvertFromHSVToRGB();
                }
                else if (e.AddedItems[0] == this.xyzTab)
                {
                    this.ConvertFromHSVToXYZ();
                }
                else
                {
                    this.ConvertFromHSVToxyY();
                }
            }
            else if (e.RemovedItems[0] == this.xyzTab)
            {
                if (e.AddedItems[0] == this.rgbTab)
                {
                    this.ConvertFromXYZToRGB();
                }
                else if (e.AddedItems[0] == this.hsvTab)
                {
                    this.ConvertFromXYZToHSV();
                }
                else
                {
                    this.ConvertFromXYZToxyY();
                }
            }
            else
            {
                if (e.AddedItems[0] == this.rgbTab)
                {
                    this.ConvertFromxyYToRGB();
                }
                else if (e.AddedItems[0] == this.hsvTab)
                {
                    this.ConvertFromxyYToHSV();
                }
                else
                {
                    this.ConvertFromxyYToXYZ();
                }
            }
            this.duringTabChange = false;
        }

        private void ConvertFromRGBToHSV()
        {
            var src = this.rgbPicker.SelectedColor;
            var rgb = Colors.ToRGBFull(src);
            this.hsvPicker.SelectedColor = Colors.ToHSV(rgb);
            this.ConversionMessage = string.Empty;
        }

        private void ConvertFromRGBToXYZ()
        {
            var src = this.rgbPicker.SelectedColor;
            var rgb = Colors.ToRGBFull(src);
            this.xyzPicker.SelectedColor = Colors.ToXYZ(rgb);
            this.ConversionMessage = string.Empty;
        }

        private void ConvertFromRGBToxyY()
        {
            this.ConvertFromRGBToXYZ();
            this.xyYPicker.SelectedColor = Colors.ToxyY(this.xyzPicker.SelectedColor);
        }

        private void ConvertFromHSVToRGB()
        {
            var src = this.hsvPicker.SelectedColor;
            var rgb = Colors.ToRGB(src);
            var safeRGB = ColorsSafe.ToRGBFromHSV(rgb);
            this.rgbPicker.SelectedColor = safeRGB.Item1;
            this.ConversionMessage = safeRGB.Item2;
        }

        private void ConvertFromHSVToXYZ()
        {
            var src = this.hsvPicker.SelectedColor;
            var rgb = Colors.ToRGB(src);
            this.xyzPicker.SelectedColor = Colors.ToXYZ(rgb);
            this.ConversionMessage = string.Empty;
        }

        private void ConvertFromHSVToxyY()
        {
            this.ConvertFromHSVToXYZ();
            this.xyYPicker.SelectedColor = Colors.ToxyY(this.xyzPicker.SelectedColor);
        }

        private void ConvertFromXYZToRGB()
        {
            var src = this.xyzPicker.SelectedColor;
            var rgb = Colors.ToRGB(src);
            var safeRGB = ColorsSafe.ToRGBFromXYZ(rgb);
            this.rgbPicker.SelectedColor = safeRGB.Item1;
            this.ConversionMessage = safeRGB.Item2;
        }

        private void ConvertFromXYZToHSV()
        {
            var src = this.xyzPicker.SelectedColor;
            var rgb = Colors.ToRGB(src);
            var safeHSV = ColorsSafe.ToHSVFromXYZ(rgb);
            this.hsvPicker.SelectedColor = safeHSV.Item1;
            this.ConversionMessage = safeHSV.Item2;
        }

        private void ConvertFromXYZToxyY()
        {
            this.xyYPicker.SelectedColor = Colors.ToxyY(this.xyzPicker.SelectedColor);
            this.ConversionMessage = string.Empty;
        }

        private void ConvertFromxyYToXYZ()
        {
            var src = this.xyYPicker.SelectedColor;
            this.xyzPicker.SelectedColor = Colors.ToXYZ(src);
            this.ConversionMessage = string.Empty;
        }

        private void ConvertFromxyYToRGB()
        {
            var src = this.xyYPicker.SelectedColor;
            this.xyzPicker.SelectedColor = Colors.ToXYZ(src);
            this.ConvertFromXYZToRGB();
        }

        private void ConvertFromxyYToHSV()
        {
            var src = this.xyYPicker.SelectedColor;
            this.xyzPicker.SelectedColor = Colors.ToXYZ(src);
            this.ConvertFromXYZToHSV();
        }

        private void UpdateSelectedColor(ColorRGB color)
        {
            this.selectedColor = color;
            this.OnPropertyChanged(nameof(SelectedColor));
        }

        private void OnSelectedColorChanged()
        {
            var rgb = Colors.ToRGBFull(this.selectedColor);
            this.rgbPicker.SelectedColor = this.SelectedColor;
            this.hsvPicker.SelectedColor = Colors.ToHSV(rgb);
            this.xyzPicker.SelectedColor = Colors.ToXYZ(rgb);
            this.OnPropertyChanged(nameof(SelectedColor));
        }
    }
}
