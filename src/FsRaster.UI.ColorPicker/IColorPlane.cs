using System.Windows;

namespace FsRaster.UI.ColorPicker
{
    public interface IColorPlane<TColor>
        : IInputElement
    {
        Point Project(TColor color);
        TColor Project(Point pt);
        TColor Coerce(TColor color);
    }
}
