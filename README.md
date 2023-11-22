# Low-level-multi-tasking-kernel-in-assembly
This project implements a basic multi tasking kernel as a TSR using pure assembly code for Intel IAPX88 architecture.
Uses [this book](https://github.com/harismuneer/BelalHashmi-Assembly-Exercise-Solutions/blob/master/Book/Assembly%20Lang%20Programming%20by%20Sir%20Belal%20Hashmi.pdf) for reference.

Hooks to the timer service routine and introduces a terminate-and-stay-resident that invokes multitasking behaviour on this hook. It takes contexts and stacks in a process control block list and switches these context upon the hook. We can load any functionality as a PCB and concurrent behaviour is obtained. Here we are using a su


