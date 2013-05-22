#load "..\src\gnuplot.fs"

open FSharp.GnuPlot

let gp = new GnuPlot()

//
Series.Lines "sin(x)"
|> gp.Plot

Series.Lines "sin(x)"
|> fun s -> gp.Plot(s, RangeX.[0.0 .. 10.0])

gp.Plot(Series.Lines [(0.,1.); (4.,5.); (9.,8.)], Range.[0.0 .. 20.0, 0.0 .. 20.0])

gp.Plot(Series.Lines [0.0 .. 10.0])

[0.0 .. 10.0]
|> Series.Lines
|> fun s -> gp.Plot(s, RangeX.[-5.0 .. 5.0])


[Series.Lines [0.0 .. 10.0];
 Series.Lines [-20.0 .. 5.0]]
 |> gp.Plot

gp.Plot([Series.Lines [0.0 .. 10.0];
         Series.Lines [-20.0 .. 5.0]], Range.[-5.0 .. 30.0, -30.0 .. 20.0])

//
gp.Plot(Series.Points "sin(x)")
gp.Plot(Series.Points "sin(x)", range = RangeX.[0.0 .. 10.0])
// axes should be reset (cleaned up)
gp.Plot(Series.Points "sin(x)")

//

//gp.SendCommand "set autoscale xy"

//
gp.Plot(Series.Points [float 0 .. float 10])
gp.Plot(Series.Points [(0.,1.); (4.,5.); (9.,8.)])
gp.Plot(Series.Points [(7.0, 3.0)])

//
gp.Plot(Series.LinesPoints [(0.,1.); (4.,5.); (9.,8.)])


//
gp.Plot(Series.Boxes [0.0 .. 10.0])
gp.Plot(Series.Boxes [(-2.0, 2.0); (-1.0, 4.0); (1.0, 1.0)], RangeY.[0.0 .. 5.0])

// 3rd element is box width
gp.Plot(Series.Boxes [(1.1, 0.9, 0.5); (2.1, 0.8, 0.5); (3.1, 0.3, 1.5)])

// overlay
gp.Plot([Series.Points [(1.1, 0.9); (2.1, 0.8); (3.1, 0.3)];
         Series.Boxes([(1.1, 0.9, 0.5); (2.1, 0.8, 0.5); (3.1, 0.3, 1.5)])],
         RangeY.[0.0 .. 1.0])

//
[for x in 0.0 .. 0.1 .. 6.0 -> sin x + cos (2.0 * x)]
|> Series.Lines
|> gp.Plot

//
[Series.Lines "sin(x)";
 Series.Lines "x";
 Series.Lines "x-(x**3)/6"]
|> gp.Plot

gp.Plot([Series.Lines "sin(x)";
 Series.Lines "x";
 Series.Lines "x-(x**3)/6"], RangeY.[-2.0 .. 2.0])


// gnuplot in action
let yearsInt = [1975;1976;1977;1978;1979;1980;1981;1982;1983;1984;1985;1986;1987;1988;1989;1990;1991;1992;1993;1994;1995]
let pqrInt = [49;52;67;53;67;46;60;50;66;70;91;133;127;136;154;127;147;146;133;144;158]
let xyzInt = [162;144;140;122;125;117;116;113;96;101;93;92;95;79;78;85;71;54;51;49;43]

let years = List.map (fun i -> float i) yearsInt
let pqr = List.map (fun i -> float i) pqrInt
let xyz = List.map (fun i -> float i) xyzInt

List.zip years pqr
|> Series.Lines
|> gp.Plot

let yearsPqr = List.zip years pqr
let yearsXyz = List.zip years xyz
gp.Plot([Series.Lines yearsPqr;
         Series.LinesPoints yearsXyz])


// with titles
gp.Plot([Series.Lines(yearsPqr, title="PQR");
         Series.LinesPoints(yearsXyz, title="XYZ")])

// scatter plot
let pqrXyz = List.zip pqr xyz
gp.Plot(Series.Points(pqrXyz, title="pqr vs. xyz"))

//let t = gp.SendCommand "show terminal"

// with fontface and size
gp.Plot(Series.Points pqrXyz, 
        xlabel=Label("pqr", "Book Antiqua"),
        ylabel=Label "xyz")

gp.Plot(Series.Points pqrXyz)
gp.Set(xlabel=Label("pqr", "Book Antiqua", 20))
gp.Refresh

gp.Set(xlabel=Label("pqr too", fontSize=10))


// use window number
//gp.SendCommand "set term wxt 1"
gp.Plot(Series.Boxes "cos(x)", output=Output.Wxt())
gp.Plot(Series.Lines "sin(x)", output=Output.Wxt(1))
gp.Plot(Series.Lines "sin(x)/(1+x)")
gp.Plot(Series.Lines "tan(x)/(1+x)", output=Output.Wxt 101)

// with png output
gp.Plot(Series.Lines "cos(x)/(1+x)",
         output=Output.Png("test.png"))

gp.Plot(Series.Lines "cos(x)/(1+x)", xlabel=Label("x label", "Book Antiqua", 20),
        ylabel=Label("y label", "Arial", 15),
        output=Output.Png("test.png"))

gp.Plot(Series.Lines "cos(x)/(1+x)", xlabel=Label("x label", "Book Antiqua", 20),
         output=Output.Wxt())


// with enhanced
gp.Plot(Series.Lines "cos(x)/(1+x)", xlabel=Label("x_a"),
         output=Output.Wxt(isEnhanced=true))
gp.Plot(Series.Lines "cos(x)/(1+x)", xlabel=Label("x_a"),
         output=Output.Wxt(isEnhanced=false))
gp.Plot(Series.Lines "cos(x)/(1+x)", xlabel=Label("x_b", "Book Antiqua", 20),
        ylabel=Label("y_d", "Arial", 15),
        output=Output.Png("test.png", isEnhanced=true))


// set font for entire window (not just labels)
// NB: font names are case-sensitive
gp.Plot(Series.Points("cos(x)", title="COS test"),
   output=Output.Wxt(fontName="Brush Script MT Italic", fontSize=25))

// set global enhanced mode, but turn off for xlabel
gp.Plot(Series.Lines "cos(x)", output=Output.Wxt(isEnhanced=true),
        ylabel=Label("label_a"))
gp.Plot(Series.Lines "x**2", output=Output.Wxt(isEnhanced=true),
        ylabel=Label("x^2", isEnhanced=false),
        xlabel=Label("label_b"))
gp.Plot(Series.LinesPoints "x**2", output=Output.Wxt(isEnhanced=true),
        ylabel=Label "a@^b_{cd}",
        xlabel=Label("{/Symbol=20 G}"))
gp.Plot(Series.LinesPoints("x**2", title="x^2"), output=Output.Png("test.png", isEnhanced=true),
        ylabel=Label "a@^b_{cd}",
        xlabel=Label("{/Symbol=30 G}"))



// multiplot
gp.Set(multiplot=MultiPlot(2,1))
gp.Plot (Series.Lines "sin(x)", xlabel=Label "x", ylabel=Label "y")
gp.Plot (Series.Points "cos(x)")
gp.Unset(multiplot=MultiPlot(2,1))


// test terminal and palette capabilities
gp.TestPalette
gp.TestTerminal

// with pointtype
gp.Plot(Series.Points([0.0 .. 0.1 .. 2.0], pointType=4))
// with pointsize
gp.Plot(Series.LinesPoints([0.0 .. 0.1 .. 2.0], pointSize=10))
gp.Plot(Series.Points([(0.,1.); (4.,5.); (9.,8.)], pointType=4, pointSize=5),
 Range.[0.0 .. 20.0, 0.0 .. 20.0])


gp.SendCommand "save terminal 'cmdLog.txt'"


//
gp.Quit


//
