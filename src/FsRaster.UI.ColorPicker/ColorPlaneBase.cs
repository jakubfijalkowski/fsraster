using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.Imaging;

namespace FsRaster.UI.ColorPicker
{
    public abstract class ColorPlaneBase<TColor>
        : Image, IColorPlane<TColor>
    {
        private readonly WriteableBitmap plane;

        private bool shouldReloadPlane = false;
        private bool isLoaded = false;

        public ColorPlaneBase(int width, int height)
        {
            this.plane = BitmapFactory.New(width, height);

            this.shouldReloadPlane = true;
            this.Source = plane;
            this.Loaded += (s, e) =>
            {
                lock (this)
                {
                    this.isLoaded = true;
                    this.GeneratePlaneInternal();
                }
            };
            this.Unloaded += (s, e) => this.isLoaded = false;
        }

        public abstract TColor Coerce(TColor color, TColor currentColor);
        public abstract TColor Project(Point pt);
        public abstract Point Project(TColor color);
        protected abstract unsafe void GeneratePlane(uint* pixels);

        internal static void OnPropertyChanged(DependencyObject dp, DependencyPropertyChangedEventArgs e)
        {
            var obj = (ColorPlaneBase<TColor>)dp;
            lock (obj)
            {
                obj.shouldReloadPlane = true;
                if (obj.isLoaded)
                {
                    obj.GeneratePlaneInternal();
                }
            }
        }

        private unsafe void GeneratePlaneInternal()
        {
            lock (this)
            {
                if (this.shouldReloadPlane)
                {
                    using (var ctx = this.plane.GetBitmapContext(ReadWriteMode.ReadWrite))
                    {
                        ctx.Clear();
                        this.GeneratePlane((uint*)ctx.Pixels);
                    }
                    this.shouldReloadPlane = false;
                }
            }
        }
    }
}
