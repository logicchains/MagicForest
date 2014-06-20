extern crate collections;

use std::cmp::Ordering;
use std::os;

struct Forest{
       goats : i32,
       wolves : i32,
       lions : i32
}

impl Eq for Forest {
    fn eq(&self, other: &Forest) -> bool { 
        other.goats == self.goats &&
	other.wolves == self.wolves && 
	other.lions == self.lions}
}

impl Clone for Forest{
    fn clone(&self) -> Forest{
       Forest{goats: self.goats, wolves: self.wolves, lions: self.lions}
    }
}

fn printForest(f : &Forest) {
     print!("Forest [goats= {}, wolves= {}, lions= {}]", f.goats, f.lions, f.wolves); 
}

fn printForests(fors : &Vec<Forest>){
   let mut i = 0;
   while i < fors.len(){
   	 printForest(fors.get(i));
	 print!("\n");
	 i+=1;
   }
}

fn forestStable(forest : &Forest) -> bool {
  if forest.goats == 0{
   return (forest.wolves == 0) || (forest.lions == 0);
  }  
  return (forest.wolves == 0) && (forest.lions == 0);
}

fn forestInvalid(forest : &Forest) ->  bool {
  return forest.goats < 0 || forest.wolves < 0 || forest.lions < 0;
} 

fn forLessThan(f1: &Forest, f2: &Forest) -> Ordering{
  if f1.goats == f2.goats{
    if f1.wolves == f2.wolves {
      if f1.lions == f2.lions {
      	 Equal
      }else{
	f1.lions.cmp(&f2.lions)
      }
    }else {
	f1.wolves.cmp(&f2.wolves)
    }
  }else{
	f1.goats.cmp(&f2.goats)
  }
}

fn meal(forests: &Vec<Forest>) -> Vec<Forest> {
    let mut mealedForests: Vec<Forest> = Vec::with_capacity(forests.len()*3);
    let mut i = 0;
    while i < forests.len() {
    	  let x = &forests.get(i);
    	  mealedForests.push(Forest{goats : x.goats -1, wolves : x.wolves -1, lions: x.lions +1});
    	  mealedForests.push(Forest{goats : x.goats -1, wolves : x.wolves +1, lions: x.lions -1});
    	  mealedForests.push(Forest{goats : x.goats +1, wolves : x.wolves -1, lions: x.lions -1});	
	  i+=1;
    }
    let (_, mut okForests) = mealedForests.clone().partition(|a| forestInvalid(a) );
    okForests.sort_by(|a,b| forLessThan(a,b) );
    okForests.dedup();
    return okForests;
}

fn devouringPossible(forests: &Vec<Forest>) -> bool {
  !(forests.len() < 1) && !(forests.iter().any(|x| forestStable(x)))
}

fn stableForests(forests: &Vec<Forest>) -> Vec<Forest> {
    let mut filteredForests: Vec<Forest> = Vec::with_capacity(3);
    let mut i = 0;
    while i < forests.len() {
    	  if forestStable(forests.get(i)){
	     let x = forests.get(i);
	     filteredForests.push(Forest{goats : x.goats, wolves : x.wolves, lions: x.lions});
	  }
	  i+=1;
    }
    filteredForests
}

fn findStableForests(forest: Forest) -> Vec<Forest>{
   let mut forests: Vec<Forest> = Vec::with_capacity(1);
   forests.push(forest);
   while devouringPossible(&forests){
    forests = meal(&forests);
  }
  return stableForests(&forests);
}

fn main(){
   let args = os::args();
   if args.len() != 4{
      print!("Error: input should be in the form of <goats> <wolves> <lions>\n");
      return;
   }  
   let initialFor = Forest{goats: std::i32::parse_bytes(args[1].clone().into_bytes(),10).expect("Error: goats not an int.\n"), 
       		          wolves: std::i32::parse_bytes(args[2].clone().into_bytes(),10).expect("Error: wolves not an int.\n"),
			  lions:  std::i32::parse_bytes(args[3].clone().into_bytes(),10).expect("Error: lions not an int.\n")};
   let stableFors = findStableForests(initialFor);
   if stableFors.len() < 1 {
      print!("No stable forests found.\n");
   }
   printForests(&stableFors);
}