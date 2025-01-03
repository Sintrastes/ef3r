# ef3r

[![codecov](https://codecov.io/github/Sintrastes/ef3r/graph/badge.svg?token=2A1XMWGOSH)](https://codecov.io/github/Sintrastes/ef3r)

Work in progress.

Ef3r, prononced "effer", as in "effervescent", (will be) a

 - Minimalistic
 - Dynamically / gradually typed
 - Functional programming language

with:

 - First-class language support for (push) Functional Reactive Programming.
 - An innovative visual debugger with "expression tracing" capabilities.
 - Universal Function Call Syntax.
 - Extensible records.

# Motivation

Consider the following Kotlin program:

```kotlin
fun main() {
    val x = 2
    
    val y = 3
    
    val z = x + y
    
    println("${1 / (z - 5)}")
}
```

If we run this, we will get a stack trace for a division by zero exception which looks like the following:

```
Exception in thread "main" java.lang.ArithmeticException: / by zero
 at FileKt.main (File.kt:8) 
 at FileKt.main (File.kt:-1) 
 at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0 (:-2) 
```

As software developers, we're used to seeing stack traces like these, and they can often be helpful in diagnosing problems, as they pinpoint the exact function where an issue occurred -- showing us where to look for the problem, as well as what functions were invoked in what our application's call graph -- giving us a bit of information about some of the control flow that led to the issue.

In this case, it's trivial to see what's wrong here -- `z` is always `5`, so of course `1 / (z - 5)` will cause a division by zero error -- but in real world applications, things are not usually this simple. Depending on the complexity of a bug, we might consider debugging to step through the program to try to figure out where the logic does not meet our expectations, adding `assert` statements and additional unit tests to make sure that invariants we expect of our code are being held, adding or analyzing existing logging statements, and so on.

Again, this is all well and good, and as professional software developers we're all used to this song and dance. But what if we add asynchronous code or multi-threading to the mix?

```kotlin
fun main() {
    val scope = CoroutineScope(Dispatchers.Default)

    val eventFlow = MutableSharedFlow<Int>()

    val job = scope.launch {
        var value = 0
        while(value <= 8) {
            eventFlow.emit(value)
            value += 1
            delay(1000)
        }
    }

    scope.launch {
        eventFlow.collect { event ->
            println("Computed value is: ${20 / (8 - event)}")
        }
    }

    runBlocking {
	job.join()
    }
}
```

Running this, our code appears to work until after 8 seconds `eventFlow` emits the value `8`, and we get a divide by zero exception:

```
Exception in thread "DefaultDispatcher-worker-3" java.lang.ArithmeticException: / by zero
	at org.example.AppKt$main$job2$1$1.emit(App.kt:22)
	at org.example.AppKt$main$job2$1$1.emit(App.kt:21)
	at kotlinx.coroutines.flow.SharedFlowImpl.collect$suspendImpl(SharedFlow.kt:392)
	at kotlinx.coroutines.flow.SharedFlowImpl$collect$1.invokeSuspend(SharedFlow.kt)
	at kotlin.coroutines.jvm.internal.BaseContinuationImpl.resumeWith(ContinuationImpl.kt:33)
	at kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:104)
	at kotlinx.coroutines.scheduling.CoroutineScheduler.runSafely(CoroutineScheduler.kt:585)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.executeTask(CoroutineScheduler.kt:802)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.runWorker(CoroutineScheduler.kt:706)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.run(CoroutineScheduler.kt:693)
	Suppressed: kotlinx.coroutines.internal.DiagnosticCoroutineContextException: [StandaloneCoroutine{Cancelling}@1a701674, Dispatchers.Default]
```

That's not so nice at all anymore. In this case there's some useful information still, and obviously in this contrived example it's very easy to figure out what's going on, but if you work with a lot of asynchronous, reactive, or functional code, you end up getting used to disapointment with stack traces. Many use this as an argument that we should avoid using some of these features and paradigms entirely.

But while the frustration with mixing stack traces with these paradigms is understandable, I think that trying to use this as a reason for ditching these paradigms entirely is a mistake. The entire concept of a stack trace was designed by and for imperative programmers. We shouldn't expect imperative tools to work with non-imperative paradigms and techniques. We should work towards building new debugging and diagnostic tools that work better with these paradigms. That's where ef3r comes in.

Ef3r comes from a long tradition of what you might call "expression-oriented" programming languages. The basic idea is that rather than trying to solve things with statements, we focus instead on building up expressions that describe whatever problem we are interested in, and manipulating those expressions (largely acting on them with mathematical functions) to produce a desired result. This is as opposed to what we might call "statement-oriented" programming languages, where we primarialy work by executing _statements_ in different orders, and manipulating the order in which those statements are executed (the control flow of our language).

You might be more familiar with the firt paradigm by it's common name: "functional" or maybe even "declarative" programming, and the second by the name "imperative programming", but I think this new terminology helps clarify the key difference between the two. Note that of course there is a lot of overlap, and the two paradigms are not mutually exlusive by any means, but we can differentiate them by the _primary mechanisms by which they solve problems_.

In light of this, it might make sense why "stack traces" are less useful for more "declarative" paradigms. Stack traces are all about control flow -- how we move through the execution of different statements in different functions within our program, and in what order. Declarative paradigms on the other hand are generally more focused on _data flow_ -- how _expressions_ flow through and are transformed by our programs. But stack traces do not give us any information of the sort!

To solve this problem, ef3r introduces the notion of an _expression trace_. Going back to our first Kotlin example, in ef3r this would look like the following:

```
x = 2;
    
y = 3;
    
z = x + y;
    
print(1 / (z - 5));
```

executing us gives the following "expression trace" showing how the data flows through our program:

```
Encountered divide-by-zero error in some_func (example.efrs:39:11).

The denominator was 0 because of the runtime values:
    at example.efrs:5  -> 2 (constant)
    at example.efrs:7  -> 3 (constant)
    at example.efrs:11 -> 5 (applied function "+")
    at example.efrs:13 -> 3 (constant)
    at example.efrs:17 -> 8 (applied function "+")
    at example.efrs:19 -> 8 (constant)
    at example.efrs:19 -> 0 (applied function "-")
```

When we get an unexpected result, this gives of a _full rundown_ of where the offending data actually came from in our program.

Again, for such a simple example this is not incredibly useful, but imagine this is an exception that occurred in a much larger application -- perhaps some legacy code you do not fully understand. 

Rather than having to play a game of detective, this immediately tells us what we need to know about the problem. 

While this does not replace the utility of all of the other debugging methods previously mentioned, what this does do is enchance the utility of "stack traces", making them far more useful in a functional, concurrent, or data-driven context.

# Asynchronous Example

To elaborate on the asynchronous Kotlin example from before: What is it that makes stack traces so unhelpful in an asynchronous context? I think this is for two reasons:

  1. The nature of an asynchronous runtime like Kotlin coroutines inherently obscures control flow.
  2. When we pass data around our application using constructs like `Flow` (reactive streams) or channels, as far as control flow is concerned, we just sent this data into a "black hole", and all context of where the offending data originally came from is entirely lost in the stack trace, whereas in a completely sequential context we may have been able to understand more from the control flow.

Expression traces solve both of these problems. Let's look at the ef3r version of the Kotlin example from earlier:

```
events = new_node(Int, None)

job = launch {
    value = var(0)
    while value <= 8 {
        events.emit(value)
        value.set(value.get() + 1)
        delay(1000)
    }
}

events.on_update { event ->
    println("Computed value is: " + display(20 / (8 - event)))
}

job.await()
```



# Philosophy

Ef3r seeks to distill the concepts of modern software development into their simplest, most fundamental and orthogonal building blocks. As such, we reject a monolithic all-encompasing programming ontology based on _classes_, which conflate many seperate concerns (nominal typing, construction, encapsulation, state, data bundling) into a single mechanism. Coming from other more mainstream languages, this leads to some interesting design descisions, such as:

 - No distinction between "functions" and "methods" (Universal Function Call Syntax).
 - Lazy evaluation (provides the mechanism for easy encapsulation, by allowing data to implicitly act like a closure).
 - No distinction between functions and properties.
 - All data is inherently extensible. (Data modeling is based off of extensible records and variants)

However, despite many of these concepts having a basis in academic programming language theory, we have no desire to be an academic language. As such, we also aim to focus on usability, and first-class tooling, and take inspiration from the following prior art:

 - [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)
 - [Whyline](https://www.cs.cmu.edu/~NatProg/whyline.html)
 - [Eve](https://witheve.com/)

We also take inspiration from:

 - [Kotlin](https://kotlinlang.org/) (lambda / "block" syntax, "when" expressions)
 - [Haskell](https://www.haskell.org/) ("lambda case" syntax, binary operator backtick syntax, `where` expressions).
 - [Purescript](https://www.purescript.org/) / [OCaml](https://ocaml.org/) (Extensible records and variants / OCaml object system).
 - [Julia](https://julialang.org/) (multiple dispatch, strong dynamic typing system).
 - [Python](https://www.python.org/) (minimal syntax for variable declarations).
 - [Rust](https://www.rust-lang.org/) (Functions are private-by-default in modules, no need for a specail `private` syntax, `?` for unwrapping effectful values).
 - [Idris](https://www.idris-lang.org/) (Types are just values, no need for a specail syntax for type application v.s. term application.

# Acronym

Experimental Functional Reactive Runtime in Rust
