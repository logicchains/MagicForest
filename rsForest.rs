extern crate collections;

use std::iter::FlatMap;

struct Forest{
       goats : i32,
       wolves : i32,
       lions : i32
}

/*fn doMeal(x : &Forest) -> Vec<Forest>{
   return vec!([Forest{goats : x.goats -1, wolves : x.wolves -1, lions: x.lions +1},	
      Forest{goats : x.goats -1, wolves : x.wolves +1, lions: x.lions -1},	
      Forest{goats : x.goats +1, wolves : x.wolves -1, lions: x.lions -1}])
}*/

fn forestStable(forest : &Forest) -> bool {
  if forest.goats == 0{
   return (forest.wolves == 0) || (forest.lions == 0);
  }  
  return (forest.wolves == 0) && (forest.lions == 0);
}

fn forestInvalid(forest : &Forest) ->  bool {
  return forest.goats < 0 || forest.wolves < 0 || forest.lions < 0;
} 

fn forLessThan(f1: &Forest, f2: &Forest) -> bool{
  let mut res : bool;
  if f1.goats == f2.goats{
    if f1.wolves == f2.wolves {
      if f1.lions == f2.lions {
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

fn meal(forests: ~[Forest]) -> ~[Forest] {
    let mut mealedForests = Vec::from_fn(forests.len(), |n| Forest{goats : 0, wolves: 0, lions: 0});	
    let mut j = 0;
    let mut i = 0;
    while i < forests.len(){
        mealedForests.get(j).goats = forests[i].goats -1;
	mealedForests.get(j).wolves = forests[i].wolves -1;
	mealedForests.get(j).lions = forests[i].lions +1;
	j+=1;
	mealedForests.get(j).goats = forests[i].goats -1;
	mealedForests.get(j).wolves = forests[i].wolves +1;
	mealedForests.get(j).lions = forests[i].lions -1;
	j+=1;
	mealedForests.get(j).goats = forests[i].goats +1;
	mealedForests.get(j).wolves = forests[i].wolves -1;
	mealedForests.get(j).lions = forests[i].lions -1;
	j+=1;
	i+=1;
    }

//  let mut it = forests.iter().flat_map(|&x|[doMeal(x)]);

/*
  return map!(a => [forest_t(-1, -1, +1)+a, forest_t(-1, +1, -1)+a, forest_t(+1, -1, -1)+a])(forests)
    .join
    .partition!(forest_invalid)
    .quickSort
    .uniq.array;*/
}

fn main(){

}