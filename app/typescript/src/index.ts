/**
 * Grokking Functional Programming - TypeScript fp-ts implementation.
 */

// Chapter 1: Introduction to Functional Programming
export {
  // 1.1 Imperative vs Functional
  calculateScoreImperative,
  wordScore,
  // 1.2 Basic function definitions
  increment,
  getFirstCharacter,
  add,
  double,
  greet,
  toUppercase,
  reverseString,
  // 1.3 Conditional functions
  absolute,
  maxValue,
  minValue,
  clamp,
  // 1.4 Predicate functions
  isEven,
  isPositive,
  isEmpty,
  // 1.5 Constants
  MAX_SCORE,
  MIN_SCORE,
  DEFAULT_GREETING,
  getBoundedScore,
  // 1.6 Function composition with pipe
  transformString,
  transformNumber,
  calculateBoundedScore,
} from './ch01_intro.js'

// Chapter 2: Pure Functions
export {
  // 2.1 Basic pure functions
  add as addPure,
  stringLength,
  wordScore as wordScorePure,
  // 2.2 Bonus score
  bonusScore,
  // 2.3 Shopping cart discount
  getDiscountPercentage,
  calculateDiscount,
  calculateFinalPrice,
  // 2.4 Tip calculation
  getTipPercentage,
  calculateTip,
  // 2.5 Referential transparency
  referentialTransparencyExample,
  // 2.6 String operations
  appendExclamation,
  countVowels,
  // 2.7 List operations
  doubleAll,
  filterPositive,
  findLongest,
  allPositive,
  anyNegative,
  sumList,
  average,
  // 2.8 Word score without 'a'
  wordScoreNoA,
  // 2.9 Higher-order functions
  applyTwice,
  compose,
  composeWithFlow,
  // 2.10 Timestamp formatting
  formatTimestamp,
} from './ch02_pure_functions.js'

// Chapter 3: Immutable Data
export {
  // 3.1 Immutable list operations
  appended,
  appendedAll,
  prepended,
  // 3.2 Slice operations
  firstN,
  firstTwo,
  lastN,
  lastTwo,
  moveFirstTwoToEnd,
  insertBeforeLast,
  insertAtMiddle,
  // 3.3 Itinerary replanning
  replan,
  // 3.4 String operations
  abbreviate,
  substring,
  // 3.5 Immutable objects
  type Point as PointCh03,
  createPoint as createPointCh03,
  withX,
  withY,
  type City,
  createCity,
  withPopulation,
  // 3.6 ReadonlySet
  addToSet,
  removeFromSet,
  // 3.7 ReadonlyMap
  updateMap,
  removeKey,
  // 3.8 fp-ts immutable operations
  appendWithFpts,
  prependWithFpts,
  concatWithFpts,
} from './ch03_immutable_data.js'

// Chapter 4: Higher-Order Functions
export {
  // 4.1 Sorting functions
  score,
  rankedWords,
  sortByLength,
  sortByLengthDesc,
  numberOfS,
  sortByNumberOfS,
  // 4.2 Map
  getLengths,
  getScores,
  doubleAll as doubleAllCh04,
  negateAll,
  // 4.3 Filter
  filterShortWords,
  filterOdd,
  filterLargerThan,
  highScoringWords,
  // 4.4 Reduce
  sumAll,
  totalLength,
  totalSCount,
  findMax,
  cumulativeScore,
  // 4.5 Functions returning functions
  largerThan,
  divisibleBy,
  shorterThan,
  containsSMoreThan,
  // 4.6 Word scoring
  bonus,
  penalty,
  wordScoreWithBonus,
  wordScoreWithBonusAndPenalty,
  highScoringWordsWithThreshold,
  // 4.7 Generic higher-order functions
  myMap,
  myFilter,
  myReduce,
  // 4.8 Programming languages
  type ProgrammingLanguage,
  createLanguage,
  sortByYear,
  getNames,
  filterYoungLanguages,
} from './ch04_higher_order.js'

// Chapter 5: flatMap
export {
  // 5.1 Flatten and flatMap
  flatten,
  flatMap,
  chainExample,
  // 5.2 Book and Movie
  type Book,
  type Movie,
  createBook,
  createMovie,
  bookAdaptations,
  getAllAuthors,
  getAllMovies,
  recommendationFeed,
  // 5.3 Friend recommendations
  recommendedBooks,
  getRecommendedAuthors,
  // 5.4 Point generation
  type Point,
  type Point3d,
  createPoint,
  createPoint3d,
  generatePoints,
  generatePoints3d,
  // 5.5 List size changes
  duplicateEach,
  withIncremented,
  filterEvenViaChain,
  filterViaChain,
  // 5.6 Option combination
  compactOptions,
  filterMapExample,
  // 5.7 sequenceT
  combineOptions,
  cartesianProduct,
  // 5.8 traverse
  traverseOption,
  // 5.9 Circle points
  isInside,
  allCombinations,
  insidePoints,
} from './ch05_flatmap.js'

// Chapter 6: Error Handling with Option
export {
  // 6.1 Option basics
  safeDivide,
  parseNumber,
  safeHead,
  safeLast,
  safeGet,
  // 6.2 TvShow parsing
  type TvShow,
  createTvShow,
  extractName,
  extractYearStart,
  extractYearEnd,
  extractSingleYear,
  parseShow,
  // 6.3 Option operations
  mapOption,
  flatMapOption,
  filterOption,
  orElseOption,
  getOrElseOption,
  // 6.4 Error handling strategies
  parseShowsBestEffort,
  parseShowsAllOrNothing,
  // 6.5 Search with Option
  findFirst,
  findLast,
  // 6.6 forall and exists
  forallOption,
  existsOption,
  // 6.7 Option utilities
  toArray,
  fromNullable,
  fromPredicate,
  // 6.8 Adding string numbers
  addStrings,
  // 6.9 Option and ReadonlyArray
  traverseArray,
  safeMax,
  safeMin,
} from './ch06_error_handling.js'

// Chapter 7: Either and ADT
export {
  // 7.1 Either basics
  safeDivideE,
  parseNumberE,
  // 7.2 TvShow parsing with Either
  type TvShow as TvShowE,
  createTvShow as createTvShowE,
  extractNameE,
  extractYearStartE,
  extractYearEndE,
  extractSingleYearE,
  parseShowE,
  // 7.3 Either operations
  mapEither,
  flatMapEither,
  orElseEither,
  getOrElseEither,
  // 7.4 Option to Either conversion
  optionToEither,
  eitherToOption,
  // 7.5 ADT - Sum types
  type MusicGenre,
  type YearsActive,
  stillActive,
  activeBetween,
  type Location,
  type Artist,
  createArtist,
  // 7.6 Pattern matching
  wasArtistActive,
  activeLength,
  // 7.7 Search conditions
  type SearchCondition,
  searchByGenre,
  searchByOrigin,
  searchByActiveYears,
  searchArtists,
  searchArtistsAny,
  // 7.8 Validation with Either
  validateAge,
  validateUsername,
  validateEmail,
  type User,
  validateUser,
  // 7.9 Either and ReadonlyArray
  traverseArrayE,
  collectRights,
  // 7.10 Payment method ADT
  type PaymentMethod,
  creditCard,
  bankTransfer,
  cash,
  describePayment,
  // 7.11 Sample data
  sampleArtists,
} from './ch07_either.js'
