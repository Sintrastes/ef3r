
// 
// efrrr is intended to be deployed as a process that can be debugged interactively.
// Thus, we will need an API for communicating with a running process for the purposes of debugging.
// 
// Example actions:
//  * Adding / removing break points.
//  * Adding / removing "exptrace" points.
//  * Adding / removing "rxtrace" points.
// 
// In addition to "standard" breakpoints, this runtime also introduces two novel concepts.
// 
// "exptrace" points are places where we decide to keep around traced (unevaluated) versions of 
//  expressions. Though for a MVP we may just do this everywhere by default.
// 
//  "rxtrace" points are points where we mark transformations on a reactive behavior that we want to
//    display as a dynamic graph visualization (basically a dynamically updating dataflow diagram as
//    the program state changes).
// 
// The idea for both is these is, rather than capturing all data by default, we can have the developer
//  specify only the "most important" information that they want to see when debugging, and change this
//  dynamically during debugging, much like breakpoints.
//
// The idea behind "exptrace"d expressions is that this "trace" of unevaluated versions of expressions
//  would be carried around even when those expressions move around the application either by imperative
//  means, or when passed around FRP pipelines.
// 
// Timetravel debugging would also probably be pretty straightforward to implement in this runtime.
// 
// To support easy interop, we'll probably use something like gRPC for communication. Maybe using
//  https://github.com/hyperium/tonic
// 
// For enabling an interactive visualization for debugging the FRP aspects of this runtime,
//  we'll need to have some integration points in the runtime where we can hook into whatever
//  platform this is being deployed to's UI capabilities.
//
// See also: https://crates.io/crates/tower-lsp
//


