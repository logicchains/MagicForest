import std.algorithm;
import std.conv;
import std.stdio;
import std.array;
import std.range;
import std.format;
import std.bitmanip;

//Compile with ldc2 -O5 -release -disable-boundscheck MagicForest.d

enum ulong BITS_FOR_HASH = 10;

struct ForMap{
  BitArray bits;

  this(ulong cap){
    this.bits.length = cap;
  }

  forest_t[] uniquify(forest_t[] fors){
    auto newFors = uninitializedArray!(forest_t[])(fors.length);
    size_t i = 0;
    foreach(f; fors){
      ulong hash = f.Hash();
      if(this.bits[hash] == false){
	this.bits[hash] = true;
	newFors[i]=f;
	i++;
      }
    }
    newFors.length=i;
    return newFors;
  }
}

struct forest_t{
  align:
  int goats;
  int wolves;
  int lions;

  forest_t opBinary(string op)(forest_t rhs) pure nothrow if (op == "+") {
    return forest_t(goats+rhs.goats, wolves+rhs.wolves, lions+rhs.lions);
  }
  ulong Hash() pure nothrow{
    ulong hash = 0;
    hash |= this.goats;
    hash <<= BITS_FOR_HASH;
    hash |= this.wolves;
    hash <<= BITS_FOR_HASH;
    hash |= this.lions;
    return hash;
  }
}

void printForest(forest_t forest) {
  writefln("Forest [goats= %d, wolves= %d, lions= %d]", forest.goats, forest.lions, forest.wolves);
}      

bool forest_stable(in immutable forest_t forest) pure nothrow {
  if (forest.goats == 0) return (forest.wolves == 0) || (forest.lions == 0);
  return (forest.wolves == 0) && (forest.lions == 0);
}

bool forest_invalid(in immutable forest_t forest) pure nothrow{
  return (forest.goats < 0 || forest.wolves < 0 || forest.lions < 0);
} 

bool forLessThan(in ref forest_t f1, in ref forest_t f2) pure nothrow{
  bool res = false;
  if(f1.goats == f2.goats){
    if(f1.wolves == f2.wolves){
      if(f1.lions == f2.lions){
	res = false;
      }else{
	res = f1.lions < f2.lions;
      }
    }else {
      res = f1.wolves < f2.wolves;
    }
  }else{
    res = f1.goats < f2.goats;
  }
  return res;
}

forest_t[] meal(forest_t[] forests) {  
  /*  ulong hashBits = BITS_FOR_HASH*3;
  ulong hashCap = 1<<hashBits;
  auto hash = ForMap(hashCap);
  return hash.uniquify(
		       map!(a => [forest_t(-1, -1, +1)+a, forest_t(-1, +1, -1)+a, forest_t(+1, -1, -1)+a])(forests)
		       .join
		       .partition!(forest_invalid)
		       .array);*/
  return map!(a => [forest_t(-1, -1, +1)+a, forest_t(-1, +1, -1)+a, forest_t(+1, -1, -1)+a])(forests)
    .join
    .partition!(forest_invalid)
    .sort!(forLessThan, SwapStrategy.stable)
    .uniq
    .array;
}

forest_t[] meal2(forest_t[] forests) {
  auto forestsAddr = forests.ptr;
  size_t forLen = forests.length;
  scope forest_t[] newFors = uninitializedArray!(forest_t[])(forLen*3);
  auto newForsAddr = newFors.ptr;
  size_t bytesToSimd = (forLen)*3*4;
  int[12] potentialMeals = [-1, -1, 1, 0, -1, 1, -1, 0, 1, -1, -1, 0];  
  asm{
      movupd XMM0, [potentialMeals];
      movupd XMM1, [potentialMeals+16];
      movupd XMM2, [potentialMeals+32];
      mov R8, forestsAddr;
      mov R9, forestsAddr;
      add R9, bytesToSimd;
      mov RCX, newForsAddr;
  loop:;
      movupd XMM3, [R8];
      movupd XMM4, [R8];
      movupd XMM5, [R8];
      paddd XMM3, XMM0;
      paddd XMM4, XMM1;
      paddd XMM5, XMM2;
      movupd [RCX], XMM3;
      movupd [RCX+12], XMM4;
      movupd [RCX+24], XMM5;
      add RCX, 36;
      add R8, 12;
      cmp R8, R9;
      jne loop;
  }
  bool[forest_t] hash;
  foreach (tree ; newFors.partition!forest_invalid){
    hash[tree] = true;
  }
  return hash.keys;
}

bool devouring_possible(in forest_t[] forests) pure nothrow {
  return !forests.empty() && !any!forest_stable(forests);
}

forest_t[] stable_forests(forest_t[] forests) {
  return filter!(a => forest_stable(a))(forests).array;
}

auto find_stable_forests(in forest_t forest){
  forest_t[] forests = [forest];
  bool[forest_t] hash;
  /*  return forests
    .recurrence!((a,n) => a[n-1].map!(a => [forest_t(-1, -1, +1)+a, forest_t(-1, +1, -1)+a, forest_t(+1, -1, -1)+a])
		     .join
		     .partition!(forest_invalid)
		     .uniq.array)
    .until!(a => !devouring_possible(a));
  */
  while(devouring_possible(forests)){
    forests = meal(forests);
  }
  return stable_forests(forests);
}

void main(string[] args){
  if(args.length != 4){
    writeln("Error: input must be three values; <goats> <wolves> <lions>");
    return;
  } 
  
  immutable forest_t initialForest = {to!int(args[1]), to!int(args[2]), to!int(args[3])};
  forest_t[] stableForests = find_stable_forests(initialForest);
 
  if (stableForests.empty()) {
    "No stable forests found.".writeln;
  }
  else {
    foreach(forest; stableForests){
      printForest(forest);
    }
  } 
}
