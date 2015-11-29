using System;
using System.Globalization;
using System.Windows.Data;
using System.Windows.Media;

namespace FsRaster.UI.ColorPicker
{
    public sealed class ColorConverter
        : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is ColorRGB && targetType == typeof(Color))
            {
                return Colors.ToUIColor((ColorRGB)value);
            }
            else if (value is Color && targetType == typeof(ColorRGB))
            {
                return Colors.ToRGB((Color)value);
            }
            return null;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return this.Convert(value, targetType, parameter, culture);
        }
    }
}
