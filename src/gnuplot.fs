// ----------------------------------------------------------------------------
// GnuPlot.fs - Provides simple wrappers for calling gnuplot from F#
// (c) Tomas Petricek (tomas@tomasp.net), MS-PL open-source license
// (c) 
// ----------------------------------------------------------------------------
namespace FSharp.GnuPlot

open System
open System.Drawing
open System.Diagnostics

/// Represents an abstract command that sets some property
/// of the plot (and allows undoing the change)
type ICommand =
  abstract Command : string
  abstract Cleanup : string

/// Utilities for formatting values and for working
/// with commands (this also contains workarounds for Mono bugs)
module InternalUtils =
  // Some generic functions are not compiled correcty
  // on Mono when using F# Interactive, so we 
  type C<'T> =
    static member FormatArg(f:'T -> string, a:option<'T>) =
      match a with
      | None -> ""
      | Some(v) -> (f v)

  type D<'T when 'T :> ICommand> =
    static member CommandList(opt:option<'T>) =
      match opt with
      | Some(cmd) -> [cmd :> ICommand]
      | None -> []

  /// Formats a value of type option<'a> as a string
  /// using the emtpy string if the value is missing
  let formatArg f a = C<_>.FormatArg(f, a)
  
  /// Formats a value of type option<float>  
  let formatNum = formatArg (sprintf "%f")

  /// Turns an option value containing some class implementing
  /// ICommand into a list containing exaclty ICommand values
  let commandList opt = D<_>.CommandList(opt)

  let formatTitle = formatArg (sprintf " title '%s'")

  let formatIsEnhanced (isEnhanced:option<bool>) = 
    if isEnhanced.IsSome && isEnhanced.Value then " enhanced"
    elif isEnhanced.IsNone then ""
    else " noenhanced"

  let formatFont (fontName:option<string>) (fontSize:option<int>) =
    if (fontName.IsSome || fontSize.IsSome) then " font \""
    else ""
    + formatArg (sprintf "%s") fontName
    + formatArg (sprintf ", %d") fontSize
    + (if (fontName.IsSome || fontSize.IsSome) then "\""
       else "")

  let formatPointType (pointType:option<int>) =
    if (pointType.IsSome) then " pointtype "
    else ""
    + formatArg (sprintf "%d") pointType

  let formatPointSize (pointSize:option<int>) =
    if (pointSize.IsSome) then " pointsize "
    else ""
    + formatArg (sprintf "%d") pointSize


open InternalUtils

module Internal =
  /// Type that represents a range for a plot (this type is not
  /// intended to be constructed directly - use 'Range.[ ... ]` instead)
  type Range(?xrange, ?yrange) =
    interface ICommand with
      member this.Command = formatArg (sprintf "set xrange %s\n") xrange + 
                            formatArg (sprintf "set yrange %s\n") yrange
      member this.Cleanup = 
                            (match xrange with
                             | Some xrange -> "set xrange restore\n"
                             | None -> "") +
                            (match yrange with
                             | Some yrange -> "set yrange restore\n"
                             | None -> "") +
                            "set autoscale xy\n"
      
  /// Type that allows elegant construction of ranges specifying both X and Y    
  type RangeImplXY() =
    member this.GetSlice(fx, tx, fy, ty) =
      Range(sprintf "[%s:%s]" (formatNum fx) (formatNum tx),
            sprintf "[%s:%s]" (formatNum fy) (formatNum ty))

  /// Type that allows elegant construction of ranges specifying only X range
  type RangeImplX() =
    member this.GetSlice(fx, tx) =
      Range(xrange = sprintf "[%s:%s]" (formatNum fx) (formatNum tx))

  /// Type that allows elegant construction of ranges specifying only Y range
  type RangeImplY() =
    member this.GetSlice(fy, ty) =
      Range(yrange = sprintf "[%s:%s]" (formatNum fy) (formatNum ty))
  
/// Module with values for elegant construction of ranges
[<AutoOpen>]
module Ranges =
    open Internal
    
    /// Ranges can be constructed using the slicing syntax.
    /// For example 'Range.[-10.0 .. , 2.0 .. 8.0]
    let Range = RangeImplXY()
    /// Ranges can be constructed using the slicing syntax.
    /// For example 'RangeX.[ .. 10.0]
    let RangeX = RangeImplX()
    /// Ranges can be constructed using the slicing syntax.
    /// For example 'RangeY.[ .. 10.0]
    let RangeY = RangeImplY()

/// Module for formatting axes labels
[<AutoOpen>]
module Labels =
  type Label(label, ?fontName, ?fontSize, ?isEnhanced) =
    let Cmd = (sprintf "\"%s\"") label
              + formatFont fontName fontSize
              + formatIsEnhanced isEnhanced
    member this.cmd = Cmd

  type LabelX(xlabel : Label) =
    interface ICommand with
      member this.Command = "set xlabel " + xlabel.cmd
      member this.Cleanup = "unset xlabel\n"

  type LabelY(ylabel : Label) =
    interface ICommand with
      member this.Command = "set ylabel " + ylabel.cmd
      member this.Cleanup = "unset ylabel\n"

  let augmentXlabel xlabel = match xlabel with
                             | Some xlabel -> Some(LabelX(xlabel))
                             | None -> None

  let augmentYlabel ylabel = match ylabel with
                             | Some ylabel -> Some(LabelY(ylabel))
                             | None -> None


/// Data that are used  as an argument to the 'Series' type
/// 'Function' is used when specifying function as a string
type Data =
  | Function of string
  | Data1D of list<float>
  | Data2D of list<float*float>
  | Data3D of list<float*float*float>


// Bind dataseries with plotstyle and other plot options
type Series(plotStyle, data, ?title, ?pointType, ?pointSize) =
  let cmd = (match data with
             | Function f -> f + " with " + plotStyle
             | Data1D _ -> " '-' using ($1) with " + plotStyle
             | Data2D _ -> " '-' using ($1):($2) with " + plotStyle
             | Data3D _ -> " '-' with " + plotStyle
             + formatPointType pointType
             + formatPointSize pointSize
             + formatTitle title)
//             | Data1D _ | Data2D _ | Data3D _ -> " '-' with " + plotStyle 
//                                                 + formatTitle title)

  member this.Command = cmd
  member this.Data = data

  /// Creates a point chart
  static member Points(func : string, ?title, ?pointType, ?pointSize) =
    Series("points", Function func, ?title=title, ?pointType=pointType, ?pointSize=pointSize)
  static member Points(data : list<float>, ?title, ?pointType, ?pointSize) =
    Series("points", Data1D data, ?title=title, ?pointType=pointType, ?pointSize=pointSize)
  static member Points(data : list<float*float>, ?title, ?pointType, ?pointSize) = 
    Series("points", Data2D data, ?title=title, ?pointType=pointType, ?pointSize=pointSize)

  /// Creates a line chart
  static member Lines(func : string, ?title) =
    Series("lines", Function func, ?title=title)
  static member Lines(data : list<float>, ?title) =
    Series("lines", Data1D data, ?title=title)
  static member Lines(data : list<float*float>, ?title) = 
    Series("lines", Data2D data, ?title=title)

  /// Creates a line chart and marks data with points
  static member LinesPoints(func : string, ?title, ?pointType, ?pointSize) =
    Series("linespoints", Function func, ?title=title, ?pointType=pointType, ?pointSize=pointSize)
  static member LinesPoints(data : list<float>, ?title, ?pointType, ?pointSize) =
    Series("linespoints", Data1D data, ?title=title, ?pointType=pointType, ?pointSize=pointSize)
  static member LinesPoints(data : list<float*float>, ?title, ?pointType, ?pointSize) = 
    Series("linespoints", Data2D data, ?title=title, ?pointType=pointType, ?pointSize=pointSize)

  /// Draws boxes of heights specified in data series
  static member Boxes(func : string, ?title) =
    Series("boxes", Function func, ?title=title)
  /// Draws boxes centered about each x and of height y.   
  static member Boxes(data : list<float>, ?title) =
    Series("boxes", Data1D data, ?title=title)
  static member Boxes(data : list<float*float>, ?title) = 
    Series("boxes", Data2D data, ?title=title)
  static member Boxes(data : list<float*float*float>, ?title) = 
    Series("boxes", Data3D data, ?title=title)


/// Various outputs that can be specified to gnuplot
type Output(cmd) =
  static member Wxt(?windowNumber, ?isEnhanced, ?fontName, ?fontSize) =
    let cmd = "set term wxt" + formatArg (sprintf " %d") windowNumber
              + formatIsEnhanced isEnhanced
              + formatFont fontName fontSize
    Output(cmd)
  
  static member Png(filename, ?isEnhanced, ?fontName, ?fontSize) =
    let cmd = "set term png"
              + formatIsEnhanced isEnhanced
              + formatFont fontName fontSize
              + "\n"
              + sprintf "set output '%s'\n" filename
              //+ "replot"
    Output(cmd)

  interface ICommand with
    member this.Command = cmd
    member this.Cleanup = "set output"

      
type MultiPlot(nRows, nCols) =
  interface ICommand with
    member this.Command = sprintf "set multiplot layout %d,%d" nRows nCols
    member this.Cleanup = "unset multiplot"


/// Provides a wrapper for calling gnuplot from F#.
///
type GnuPlot(?path) =
  // Start gnuplot process when class is instantiated.
  let path = defaultArg path "gnuplot"
  let gp =
    new ProcessStartInfo
      (FileName = path, UseShellExecute = false, Arguments = "",
       RedirectStandardError = true, CreateNoWindow = true,
       RedirectStandardOutput = true, RedirectStandardInput = true)
     |> Process.Start

  // Provide event for reading gnuplot messages
  let msgEvent =
    Event.merge gp.OutputDataReceived gp.ErrorDataReceived
    |> Event.map (fun de -> de.Data)

  do
    gp.BeginOutputReadLine()
    gp.BeginErrorReadLine()
    gp.EnableRaisingEvents <- true
    let nLines = ref 0
    Event.add (fun (str : string) ->
                // First command to gnuplot results in an 'invalid character' error.
                // Ignore this.  Then start printing errors.
                if !nLines < 4 then 
                  incr nLines
                else
                  printfn "%s" str
              ) msgEvent

    
  // Send command to gnuplot process
  let sendCommand(str : string) =
    gp.StandardInput.Write(str + "\n")

  // First command to gnuplot results in an 'invalid character' error.
  // So, flush on instantiation.
  // This message is suppressed by the error message event handler.
  do
    sendCommand ""
  

  // Dispose of running process when wrapper is disposed
  // The following bits implement proper 'disposal' pattern
  member private this.Dispose(disposing) =
    gp.Kill()
    if disposing then gp.Dispose()

  override this.Finalize() =
    this.Dispose(false)

  interface System.IDisposable with
    member this.Dispose() =
      this.Dispose(true)
      System.GC.SuppressFinalize(this)

  
  /// Sends a command string to gnuplot process
  member this.SendCommand(str) = sendCommand(str)

  member this.Quit = this.SendCommand "quit"

  /// Causes all graph-related options to take on default values.
  /// Does not affect: 'set term' 'set output'
  member this.Reset = this.SendCommand "reset"

  /// Reformats and redraws current plot using data already read in.
  member this.Refresh = this.SendCommand "refresh"

  /// Graphically tests and presents terminal capabilities.
  /// Useful to find out available point and line types, fill patterns, etc.
  member this.TestTerminal = this.SendCommand "test terminal"

  /// Graphically tests and presents palette capabilities
  member this.TestPalette = this.SendCommand "test palette"

  /// The set command can be used to set _lots_ of options.
  /// No screen is drawn, however, until a plot command is given. 
  member this.Set(?range:Internal.Range, ?xlabel:Label, ?ylabel:Label,
                  ?output:Output, ?multiplot:MultiPlot) =
    let commands = List.concat [ commandList output; commandList multiplot; commandList range;
                                 commandList (augmentXlabel xlabel);
                                 commandList (augmentYlabel ylabel) ]
    for cmd in commands do
      printfn "%s" cmd.Command
      this.SendCommand cmd.Command

  /// Reset settings previously set (used mainly internally)
  member this.Unset(?range:Internal.Range, ?xlabel:Label, ?ylabel:Label,
                    ?output:Output, ?multiplot:MultiPlot) =
    let commands = List.concat [commandList output; commandList multiplot; commandList range;
                                commandList (augmentXlabel xlabel);
                                commandList (augmentYlabel ylabel) ]
    for cmd in commands do
      if "" <> cmd.Cleanup then this.SendCommand(cmd.Cleanup)

  /// Draw a plot of a single data series. For example:
  /// Create a simple line plot
  /// gp.Plot(Series.Lines [2.0; 1.0; 2.0; 5.0],
  ///           range = RangeY.[-1.0 ..])   
  ///  
  member this.Plot(data:Series, ?range, ?xlabel, ?ylabel, ?output) =
    this.Plot([data], ?range=range, ?xlabel=xlabel, ?ylabel=ylabel, ?output=output)

  /// Draw a plot consisting of multiple data series. For example:
  /// Create a simple line plot
  /// gp.Plot
  /// [ Series.Lines(title="Lines", data=[2.0; 1.0; 2.0; 5.0])
  ///   Series.Histogram(fill=Solid, data=[2.0; 1.0; 2.0; 5.0]) ]
  ///    
  member this.Plot(data:seq<Series>, ?range, ?xlabel, ?ylabel, ?output) =
    this.Set(?range=range, ?xlabel=xlabel, ?ylabel=ylabel, ?output=output)
    let cmd = "plot " +
              ([for s in data -> s.Command] |> String.concat ", \\\n")
    //printfn "Command : \n%s" cmd
    this.SendCommand cmd
    // next send data
    for s in data do
      match s.Data with
      | Function _ -> ()
      | Data1D d -> for p in d do
                      this.SendCommand (string p)
                    this.SendCommand "e"
      | Data2D d -> for p in d do
                      this.SendCommand ((string(fst p)) + " " + string(snd p))
                    this.SendCommand "e"
      | Data3D d -> for p in d do
                      let (f, s, t) = p
                      this.SendCommand ((string f) + " " + (string s)  + " " + (string t))
                    this.SendCommand "e"
    this.Unset(?range = range, ?xlabel=xlabel, ?ylabel=ylabel, ?output=output)
    
