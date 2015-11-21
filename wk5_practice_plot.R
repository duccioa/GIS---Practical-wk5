wards.f <- fortify(wards, region = "GSS_CODE")
g <- ggplot(wards.f, aes(long, lat, group = group)) + geom_polygon() + coord_equal()
g <- g + geom_line(data = tube, aes(X.from, Y.from, group = Tube.Line, col = Tube.Line))
g <- g + geom_point(data = stations, aes(OS.X, OS.Y, group = Station))
g





