#lang scribble/manual

@title[#:tag '("rackmora" "remora")]{Array-oriented programming}

@author[(author+email "Justin Slepak" "jrslepak@ccs.neu.edu" #:obfuscate? #t)]

@;{maybe add a bit about motivation here?}

@;{TODO: sections:
   1. pieces of an array
   shape, atoms;
   rank;
   scalar/atom distinction;
   syntax for arrays
   
   2. pieces of a function
   arguments, their ranks;
   function body;
   syntax for functions
   
   3. function appliction
   frame-of-cells decomposition;
   prefix agreement;
   examples w/ increasing principal frame rank;
   reranking
   
   4. boxes for irregular data
   functions w/ indeterminate result shape (like iota);
   unbox syntax;
   unsafe-unbox function
   
   5. racket integration
   importing & exporting in #lang mode;
   library/EDSL mode}

