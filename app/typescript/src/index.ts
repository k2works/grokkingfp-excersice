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
