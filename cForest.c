#include <stdio.h>
#include <stdlib.h>

struct Forest{
  int goats;
  int wolves;
  int lions;
  int hash;
};

char* toString(struct Forest* forest) {
  char* str= malloc(1000);
  snprintf(str, 1000, "Forest [goats= %d, wolves= %d, lions= %d]",
	   forest->goats, forest->lions, forest->wolves);
  return str;
}      

void PrintFor(struct Forest* forest){
  char* txt = toString(forest);
  printf("%s\n", txt);
  free(txt);
}

int hashCode(struct Forest* forest){
  int magic = 0x9e3779b9;
  int seed = 0;
  seed ^= forest->goats + magic + (seed << 6) + (seed >> 2);
  seed ^= forest->lions + magic + (seed << 6) + (seed >> 2);
  seed ^= forest->wolves + magic + (seed << 6) + (seed >> 2);
  return seed;
}

struct Forest* makeForest(int goats, int wolves, int lions) {
  struct Forest *aForest = malloc(sizeof(struct Forest));
  aForest->goats = goats;
  aForest->wolves = wolves;
  aForest->lions = lions;
  aForest->hash = hashCode(aForest);
  return aForest;
}

struct ForList{
  struct Forest* fors;
  int cap;
  int len;
};

void FreeForList(struct ForList* list){
  free(list->fors);
  free(list);
}

struct ForList* NewForList(int cap){
  struct ForList* list = malloc(sizeof(struct ForList)); 
  list->cap = cap;
  list->len = 0;
  list->fors = malloc(sizeof(struct Forest)*cap);
  return list;
}

void Add(struct ForList *list, struct Forest forest){
  if(list->len == list->cap){
    list->fors = realloc(list->fors, list->len*2*sizeof(struct Forest));
    list->cap *= 2;
  }
  list->fors[list->len] = forest;
  list->len++;
} 

int isStable(struct Forest* forest){
  if (forest->goats == 0) return (forest->wolves == 0) || (forest->lions == 0);
  return (forest->wolves == 0) && (forest->lions == 0);
}	
	
int equals(struct Forest* forest, struct Forest* oth){
  return forest->hash == oth->hash; 
}

int Contains(struct ForList* list, struct Forest* forest){
  int i;
  for(i = 0; i < list->len; i++){
    if(forest->hash == list->fors[i].hash){
      return 1;
    }
  }
  return 0;
}

void PrintFors(struct ForList* fors){
  int i;
  for(i = 0; i < fors->len; i++){
    PrintFor(&fors->fors[i]);
  }
}

int forLessThan(struct Forest* a, struct Forest* b){
  int res = 0;
  if(a->goats == b->goats){
    if(a->wolves == b->wolves){
      if(a->lions == b->lions){
	res = 0;
      }else{
	res = a->lions < b->lions;
      }
    }
    else {
      res = a->wolves < b->wolves;
    }
  }else{
    res = a->goats < b->goats;
  }
  return res;
}

void swapFors(struct Forest* a, struct Forest* b){
  struct Forest tmp;
  tmp = *a;
  *a = *b;
  *b = tmp;
}

void qsPartition(struct Forest* start, struct Forest* end, int len){
  struct Forest* mid = start + len/2;
  struct Forest* mid2 = mid;
  while(start < mid2 && mid < end){
    if(forLessThan(mid, start)){
      swapFors(start, mid);
      mid = mid + 1;
    }else{
      start = start + 1;
    }
  }
}

void qsFors(struct Forest* forests, int len){
  if (len < 2){
    return;
  }
  struct Forest p = forests[len / 2];
  struct Forest* left = forests;
  struct Forest* right = forests + len - 1;
  while (left <= right) {
    if (forLessThan(left, &p)) {
      left++;
      continue;
    }
    if (forLessThan(&p, right)) {
      right--;
      continue;
    }
    swapFors(left, right);
    left++;
    right--;
  }
  qsFors(forests, right - forests + 1);
  qsFors(left, forests + len - left);
}

void qsFors1(struct Forest* forests, int len){
  if(len < 2){
    return;
  }
  int partition = len/2;
  qsPartition(forests, forests + len, len);
  qsFors(forests, partition-1);
  qsFors(forests+partition+1, len-partition);
}

struct ForList* uniqueFors(struct ForList* forests){
  int i, j=0;
  struct ForList* uForests = NewForList(forests->len);
  /*  printf("Forests:\n");
      PrintFors(forests);*/
  qsFors(forests->fors, forests->len);
  struct Forest* prevFor = &forests->fors[0];
  uForests->fors[j] = *prevFor;
  uForests->len++;
  j++;
  for(i = 1; i < forests->len; i++){
    if(!equals(prevFor, &forests->fors[i])){
      uForests->fors[j] = forests->fors[i];
      uForests->len++;
      j++;
      prevFor = &forests->fors[i];
    }
  }
  FreeForList(forests);
  /*  printf("Unique, they are:\n");
      PrintFors(uForests);*/
  return uForests;
}

struct ForList* meals(struct ForList* forests){
  int i, j = 0;
  struct ForList* mealedForests = NewForList(forests->len*3);
  for(i = 0; i < forests->len; i++){
    mealedForests->fors[j].goats = forests->fors[i].goats -1;
    mealedForests->fors[j].wolves = forests->fors[i].wolves -1;
    mealedForests->fors[j].lions = forests->fors[i].lions +1; 
    mealedForests->fors[j].hash = hashCode(&mealedForests->fors[j]);
    j++;
    mealedForests->fors[j].goats = forests->fors[i].goats -1;
    mealedForests->fors[j].wolves = forests->fors[i].wolves +1;
    mealedForests->fors[j].lions = forests->fors[i].lions -1; 
    mealedForests->fors[j].hash = hashCode(&mealedForests->fors[j]);
    j++;
    mealedForests->fors[j].goats = forests->fors[i].goats +1;
    mealedForests->fors[j].wolves = forests->fors[i].wolves -1;
    mealedForests->fors[j].lions = forests->fors[i].lions -1; 
    mealedForests->fors[j].hash = hashCode(&mealedForests->fors[j]);
    j++;
    mealedForests->len = mealedForests->len + 3;
  }
  FreeForList(forests);
  return uniqueFors(mealedForests);
  /*  struct ForList* uniqueForests = NewForList(mealedForests->len);
  j = 0;
  for(i = 0; i < mealedForests->len; i++){
    if(!Contains(uniqueForests, &mealedForests->fors[i])){
      uniqueForests->fors[j]=mealedForests->fors[i];
      uniqueForests->len = uniqueForests->len + 1;
      j++;
    }
  }
  FreeForList(mealedForests);
  qsFors(uniqueForests);
  return uniqueForests;*/
}

    
int devouringPossible(struct ForList* forests) {
  int i;
  if(forests->len){
    for(i = 0; i < forests->len; i++){
      if(isStable(&forests->fors[i])){
	return 0;
      }      
    }
    return 1;
  }else{
    return 0;
  }
}

struct ForList* stableForests(struct ForList* forests) {
  int i, j=0;
  struct ForList* stableForests = NewForList(forests->len);
  for(i = 0; i < forests->len; i++){
    if(isStable(&forests->fors[i])){
      stableForests->fors[j] = forests->fors[i];
      stableForests->len = stableForests->len + 1;
      j++;
    }
  }
  FreeForList(forests);
  return stableForests;
}
    
struct ForList* findStableForests(struct Forest forest) {
  struct ForList* forests = NewForList(3);
  forests->fors[0] = forest;
  forests->len = 1;
  while (devouringPossible(forests)){
    forests = meals(forests);
  }
  return stableForests(forests);
}

int main(int argc, char** argv) {
  int i;
  if (argc != 4) {
    printf("Error: needs three arguments; goats, wolves, lions.\n");
    exit(EXIT_FAILURE);
  }
  struct Forest *initialForest = makeForest(atoi(argv[1]) ,atoi(argv[2]), atoi(argv[3]));
  struct ForList *stableForests = findStableForests(*initialForest);
  if (!stableForests->len) {
    printf("no stable forests found.\n");
  }else {
    printf("Stable forest found:\n");
    for(i = 0; i < stableForests->len; i++){
      char* forestTxt = toString(&stableForests->fors[i]);
      printf("%s\n", forestTxt);
      free(forestTxt);
    }
    FreeForList(stableForests);
  }
}
