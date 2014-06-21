extern crate collections;

use std::os;
use std::fmt;

#[deriving(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Forest {
    goats : i32,
    wolves : i32,
    lions : i32,
}

impl Forest {
    pub fn is_stable(&self) -> bool {
        match *self {
            Forest { goats: 0, wolves: 0, lions: _ } |
            Forest { goats: 0, wolves: _, lions: 0 } |
            Forest { goats: _, wolves: 0, lions: 0 }
              => true,
            _ => false
        }
    }

    pub fn is_valid(&self) -> bool {
        self.goats >= 0 && self.wolves >= 0 && self.lions >= 0
    }
}

impl fmt::Show for Forest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Forest [goats: {}, wolves= {}, lions= {}]", self.goats, self.wolves, self.lions)
    }
}

fn printForests(fors : &[Forest]){
    for f in fors.iter() {
        println!("{}", f);
    }
}

fn meal(forests: Vec<Forest>) -> Vec<Forest> {
    let mut mealedForests: Vec<Forest> = Vec::with_capacity(forests.len()*3);
    for x in forests.move_iter() {
        mealedForests.push(Forest{goats : x.goats -1, wolves : x.wolves -1, lions: x.lions +1});
        mealedForests.push(Forest{goats : x.goats -1, wolves : x.wolves +1, lions: x.lions -1});
        mealedForests.push(Forest{goats : x.goats +1, wolves : x.wolves -1, lions: x.lions -1});
    }
    let mut okForests: Vec<Forest> = mealedForests.move_iter().filter(|f| f.is_valid()).collect();
    okForests.sort();
    okForests.dedup();
    return okForests;
}

fn devouringPossible(forests: &[Forest]) -> bool {
    !(forests.len() < 1) && !(forests.iter().any(|x| x.is_stable()))
}

fn stableForests(forests: Vec<Forest>) -> Vec<Forest> {
    forests.move_iter().filter(|f| f.is_stable()).collect()
}

fn findStableForests(forest: Forest) -> Vec<Forest>{
    let mut forests: Vec<Forest> = Vec::with_capacity(1);
    forests.push(forest);
    while devouringPossible(forests.as_slice()){
        forests = meal(forests);
    }
    return stableForests(forests);
}

fn main(){
    let args = os::args();
    if args.len() != 4{
        print!("Error: input should be in the form of <goats> <wolves> <lions>\n");
        return;
    }
    let initialFor = Forest {
        goats: from_str(args.get(1).as_slice()).expect("Goats must be an int"),
        wolves: from_str(args.get(2).as_slice()).expect("Wolves must be an int"),
        lions: from_str(args.get(3).as_slice()).expect("Lions must be an int")
    };

    let stableFors = findStableForests(initialFor);
    if stableFors.len() < 1 {
        print!("No stable forests found.\n");
    }
    printForests(stableFors.as_slice());
}
