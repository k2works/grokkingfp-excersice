/**
 * Chapter 11 & 12: Practical Application Building and Testing
 *
 * This module implements a TravelGuide application demonstrating:
 * - Domain modeling with branded types
 * - DataAccess abstraction
 * - Resource management pattern
 * - Cache implementation with Ref
 * - SearchReport for observability
 * - Property-based testing concepts
 */

import * as T from 'fp-ts/Task'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as A from 'fp-ts/ReadonlyArray'
import { pipe } from 'fp-ts/function'

// =============================================================================
// 11.1 Domain Model - Branded Types
// =============================================================================

/**
 * LocationId - branded type for type-safe location identifiers
 */
export type LocationId = string & { readonly _brand: unique symbol }

export const LocationId = {
  of: (value: string): LocationId => value as LocationId,
  value: (id: LocationId): string => id,
}

/**
 * Location - represents a geographical location
 */
export interface Location {
  readonly id: LocationId
  readonly name: string
  readonly population: number
}

export const createLocation = (
  id: string,
  name: string,
  population: number
): Location => ({
  id: LocationId.of(id),
  name,
  population,
})

/**
 * Attraction - represents a tourist attraction
 */
export interface Attraction {
  readonly name: string
  readonly description: O.Option<string>
  readonly location: Location
}

export const createAttraction = (
  name: string,
  description: O.Option<string>,
  location: Location
): Attraction => ({
  name,
  description,
  location,
})

/**
 * MusicArtist - represents a music artist from a location
 */
export interface MusicArtist {
  readonly name: string
  readonly genre: O.Option<string>
}

export const createMusicArtist = (
  name: string,
  genre: O.Option<string> = O.none
): MusicArtist => ({
  name,
  genre,
})

/**
 * Movie - represents a movie about a location
 */
export interface Movie {
  readonly name: string
  readonly year: O.Option<number>
}

export const createMovie = (
  name: string,
  year: O.Option<number> = O.none
): Movie => ({
  name,
  year,
})

/**
 * Hotel - represents a hotel near a location
 */
export interface Hotel {
  readonly name: string
  readonly rating: number
  readonly location: Location
}

export const createHotel = (
  name: string,
  rating: number,
  location: Location
): Hotel => ({
  name,
  rating,
  location,
})

// =============================================================================
// 11.2 SearchReport - for observability and testability
// =============================================================================

/**
 * SearchReport - tracks search statistics and errors
 */
export interface SearchReport {
  readonly attractionsSearched: number
  readonly artistsFound: number
  readonly moviesFound: number
  readonly errors: readonly string[]
}

export const createSearchReport = (
  attractionsSearched: number = 0,
  artistsFound: number = 0,
  moviesFound: number = 0,
  errors: readonly string[] = []
): SearchReport => ({
  attractionsSearched,
  artistsFound,
  moviesFound,
  errors,
})

export const emptySearchReport: SearchReport = createSearchReport()

export const combineSearchReports = (
  a: SearchReport,
  b: SearchReport
): SearchReport => ({
  attractionsSearched: a.attractionsSearched + b.attractionsSearched,
  artistsFound: a.artistsFound + b.artistsFound,
  moviesFound: a.moviesFound + b.moviesFound,
  errors: [...a.errors, ...b.errors],
})

/**
 * TravelGuide - the main result type combining attraction with related info
 */
export interface TravelGuide {
  readonly attraction: Attraction
  readonly subjects: readonly string[]
  readonly searchReport: SearchReport
}

export const createTravelGuide = (
  attraction: Attraction,
  subjects: readonly string[],
  searchReport: SearchReport
): TravelGuide => ({
  attraction,
  subjects,
  searchReport,
})

// =============================================================================
// 11.3 Attraction Ordering
// =============================================================================

export type AttractionOrdering = 'ByName' | 'ByLocationPopulation'

export const ByName: AttractionOrdering = 'ByName'
export const ByLocationPopulation: AttractionOrdering = 'ByLocationPopulation'

export const compareAttractions = (
  ordering: AttractionOrdering
): ((a: Attraction, b: Attraction) => number) => {
  switch (ordering) {
    case 'ByName':
      return (a, b) => a.name.localeCompare(b.name)
    case 'ByLocationPopulation':
      return (a, b) => b.location.population - a.location.population
  }
}

// =============================================================================
// 11.4 DataAccess Interface
// =============================================================================

/**
 * DataAccess - interface for data access operations
 * All methods return Task or TaskEither for purity
 */
export interface DataAccess {
  readonly findAttractions: (
    name: string,
    ordering: AttractionOrdering,
    limit: number
  ) => T.Task<readonly Attraction[]>

  readonly findArtistsFromLocation: (
    locationId: LocationId,
    limit: number
  ) => T.Task<E.Either<string, readonly MusicArtist[]>>

  readonly findMoviesAboutLocation: (
    locationId: LocationId,
    limit: number
  ) => T.Task<E.Either<string, readonly Movie[]>>

  readonly findHotelsNearLocation: (
    locationId: LocationId,
    limit: number
  ) => T.Task<E.Either<string, readonly Hotel[]>>
}

// =============================================================================
// 11.5 Resource Pattern
// =============================================================================

/**
 * Resource - pattern for safe resource acquisition and release
 */
export interface Resource<A> {
  readonly use: <B>(f: (a: A) => T.Task<B>) => T.Task<B>
}

export const makeResource = <A>(
  acquire: T.Task<A>,
  release: (a: A) => T.Task<void>
): Resource<A> => ({
  use: <B>(f: (a: A) => T.Task<B>): T.Task<B> =>
    async () => {
      const resource = await acquire()
      try {
        return await f(resource)()
      } finally {
        await release(resource)()
      }
    },
})

/**
 * ResourceEither - Resource that can fail during acquisition
 */
export interface ResourceEither<E, A> {
  readonly use: <B>(f: (a: A) => TE.TaskEither<E, B>) => TE.TaskEither<E, B>
}

export const makeResourceEither = <E, A>(
  acquire: TE.TaskEither<E, A>,
  release: (a: A) => T.Task<void>
): ResourceEither<E, A> => ({
  use: <B>(f: (a: A) => TE.TaskEither<E, B>): TE.TaskEither<E, B> =>
    async () => {
      const acquireResult = await acquire()
      if (E.isLeft(acquireResult)) {
        return acquireResult
      }
      const resource = acquireResult.right
      try {
        return await f(resource)()
      } finally {
        await release(resource)()
      }
    },
})

/**
 * Bracket pattern - acquire, use, release
 */
export const bracket = <A, B>(
  acquire: T.Task<A>,
  use: (a: A) => T.Task<B>,
  release: (a: A) => T.Task<void>
): T.Task<B> => makeResource(acquire, release).use(use)

export const bracketEither = <E, A, B>(
  acquire: TE.TaskEither<E, A>,
  use: (a: A) => TE.TaskEither<E, B>,
  release: (a: A) => T.Task<void>
): TE.TaskEither<E, B> => makeResourceEither(acquire, release).use(use)

// =============================================================================
// 11.6 Ref-based Cache
// =============================================================================

/**
 * CacheRef - atomic reference for caching
 */
export interface CacheRef<K, V> {
  readonly get: (key: K) => T.Task<O.Option<V>>
  readonly set: (key: K, value: V) => T.Task<void>
  readonly getOrCompute: (key: K, compute: T.Task<V>) => T.Task<V>
  readonly clear: () => T.Task<void>
  readonly size: () => T.Task<number>
}

export const createCacheRef = <K, V>(): CacheRef<K, V> => {
  const cache = new Map<K, V>()

  return {
    get: (key: K) => () => Promise.resolve(O.fromNullable(cache.get(key))),
    set: (key: K, value: V) => () => {
      cache.set(key, value)
      return Promise.resolve()
    },
    getOrCompute: (key: K, compute: T.Task<V>) => async () => {
      const existing = cache.get(key)
      if (existing !== undefined) {
        return existing
      }
      const value = await compute()
      cache.set(key, value)
      return value
    },
    clear: () => () => {
      cache.clear()
      return Promise.resolve()
    },
    size: () => () => Promise.resolve(cache.size),
  }
}

/**
 * Create a cached version of DataAccess
 */
export const cachedDataAccess = (
  dataAccess: DataAccess
): T.Task<DataAccess> => async () => {
  const attractionsCache = createCacheRef<string, readonly Attraction[]>()
  const artistsCache = createCacheRef<
    string,
    E.Either<string, readonly MusicArtist[]>
  >()
  const moviesCache = createCacheRef<
    string,
    E.Either<string, readonly Movie[]>
  >()
  const hotelsCache = createCacheRef<
    string,
    E.Either<string, readonly Hotel[]>
  >()

  return {
    findAttractions: (name, ordering, limit) => {
      const key = `${name}-${ordering}-${limit}`
      return attractionsCache.getOrCompute(
        key,
        dataAccess.findAttractions(name, ordering, limit)
      )
    },
    findArtistsFromLocation: (locationId, limit) => {
      const key = `${LocationId.value(locationId)}-${limit}`
      return artistsCache.getOrCompute(
        key,
        dataAccess.findArtistsFromLocation(locationId, limit)
      )
    },
    findMoviesAboutLocation: (locationId, limit) => {
      const key = `${LocationId.value(locationId)}-${limit}`
      return moviesCache.getOrCompute(
        key,
        dataAccess.findMoviesAboutLocation(locationId, limit)
      )
    },
    findHotelsNearLocation: (locationId, limit) => {
      const key = `${LocationId.value(locationId)}-${limit}`
      return hotelsCache.getOrCompute(
        key,
        dataAccess.findHotelsNearLocation(locationId, limit)
      )
    },
  }
}

// =============================================================================
// 11.7 TravelGuide Application Logic
// =============================================================================

/**
 * Build a TravelGuide for a given attraction name
 */
export const travelGuide = (
  data: DataAccess,
  attractionName: string,
  artistLimit: number = 2,
  movieLimit: number = 2
): T.Task<O.Option<TravelGuide>> => async () => {
  const attractions = await data.findAttractions(
    attractionName,
    ByLocationPopulation,
    1
  )()

  if (attractions.length === 0) {
    return O.none
  }

  const attraction = attractions[0]
  const locationId = attraction.location.id

  const [artistsResult, moviesResult] = await Promise.all([
    data.findArtistsFromLocation(locationId, artistLimit)(),
    data.findMoviesAboutLocation(locationId, movieLimit)(),
  ])

  const errors: string[] = []
  let artists: readonly MusicArtist[] = []
  let movies: readonly Movie[] = []

  if (E.isLeft(artistsResult)) {
    errors.push(artistsResult.left)
  } else {
    artists = artistsResult.right
  }

  if (E.isLeft(moviesResult)) {
    errors.push(moviesResult.left)
  } else {
    movies = moviesResult.right
  }

  const subjects = [
    ...artists.map((a) => a.name),
    ...movies.map((m) => m.name),
  ]

  const searchReport = createSearchReport(
    attractions.length,
    artists.length,
    movies.length,
    errors
  )

  return O.some(createTravelGuide(attraction, subjects, searchReport))
}

/**
 * Build TravelGuides for multiple attractions
 */
export const travelGuides = (
  data: DataAccess,
  attractionName: string,
  limit: number = 3,
  artistLimit: number = 2,
  movieLimit: number = 2
): T.Task<readonly TravelGuide[]> => async () => {
  const attractions = await data.findAttractions(
    attractionName,
    ByLocationPopulation,
    limit
  )()

  if (attractions.length === 0) {
    return []
  }

  const guides = await Promise.all(
    attractions.map(async (attraction) => {
      const locationId = attraction.location.id

      const [artistsResult, moviesResult] = await Promise.all([
        data.findArtistsFromLocation(locationId, artistLimit)(),
        data.findMoviesAboutLocation(locationId, movieLimit)(),
      ])

      const errors: string[] = []
      let artists: readonly MusicArtist[] = []
      let movies: readonly Movie[] = []

      if (E.isLeft(artistsResult)) {
        errors.push(artistsResult.left)
      } else {
        artists = artistsResult.right
      }

      if (E.isLeft(moviesResult)) {
        errors.push(moviesResult.left)
      } else {
        movies = moviesResult.right
      }

      const subjects = [
        ...artists.map((a) => a.name),
        ...movies.map((m) => m.name),
      ]

      const searchReport = createSearchReport(1, artists.length, movies.length, errors)

      return createTravelGuide(attraction, subjects, searchReport)
    })
  )

  return guides
}

/**
 * Get TravelGuide with hotels
 */
export const travelGuideWithHotels = (
  data: DataAccess,
  attractionName: string,
  hotelLimit: number = 3
): T.Task<O.Option<TravelGuide & { readonly hotels: readonly Hotel[] }>> =>
  async () => {
    const guideOption = await travelGuide(data, attractionName)()

    if (O.isNone(guideOption)) {
      return O.none
    }

    const guide = guideOption.value
    const locationId = guide.attraction.location.id

    const hotelsResult = await data.findHotelsNearLocation(
      locationId,
      hotelLimit
    )()

    const hotels = E.isRight(hotelsResult) ? hotelsResult.right : []
    const errors = E.isLeft(hotelsResult) ? [hotelsResult.left] : []

    return O.some({
      ...guide,
      hotels,
      searchReport: combineSearchReports(
        guide.searchReport,
        createSearchReport(0, 0, 0, errors)
      ),
    })
  }

// =============================================================================
// 11.8 Test Data Access (Stub)
// =============================================================================

/**
 * Create a test DataAccess with configurable data
 */
export interface TestDataConfig {
  readonly attractions?: readonly Attraction[]
  readonly artists?: readonly MusicArtist[]
  readonly movies?: readonly Movie[]
  readonly hotels?: readonly Hotel[]
  readonly artistsError?: string
  readonly moviesError?: string
  readonly hotelsError?: string
}

export const createTestDataAccess = (
  config: TestDataConfig = {}
): DataAccess => {
  const {
    attractions = [],
    artists = [],
    movies = [],
    hotels = [],
    artistsError,
    moviesError,
    hotelsError,
  } = config

  return {
    findAttractions: (name, ordering, limit) => async () => {
      const filtered = attractions.filter((a) =>
        a.name.toLowerCase().includes(name.toLowerCase())
      )
      const sorted = [...filtered].sort(compareAttractions(ordering))
      return sorted.slice(0, limit)
    },

    findArtistsFromLocation: (_locationId, limit) => async () => {
      if (artistsError) {
        return E.left(artistsError)
      }
      return E.right(artists.slice(0, limit))
    },

    findMoviesAboutLocation: (_locationId, limit) => async () => {
      if (moviesError) {
        return E.left(moviesError)
      }
      return E.right(movies.slice(0, limit))
    },

    findHotelsNearLocation: (_locationId, limit) => async () => {
      if (hotelsError) {
        return E.left(hotelsError)
      }
      return E.right(hotels.slice(0, limit))
    },
  }
}

// =============================================================================
// 12.1 Property-based Testing Helpers
// =============================================================================

/**
 * Generator type for property-based testing
 */
export type Generator<A> = () => A

/**
 * Simple random generators for property-based testing
 */
export const Gen = {
  /**
   * Generate a random integer in range [min, max]
   */
  int: (min: number, max: number): Generator<number> => () =>
    Math.floor(Math.random() * (max - min + 1)) + min,

  /**
   * Generate a random positive integer
   */
  posInt: (max: number = 1000000): Generator<number> => () =>
    Math.floor(Math.random() * max) + 1,

  /**
   * Generate a random alphanumeric string
   */
  alphaNumStr: (maxLength: number = 10): Generator<string> => () => {
    const chars =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
    const length = Math.floor(Math.random() * maxLength) + 1
    return Array.from(
      { length },
      () => chars[Math.floor(Math.random() * chars.length)]
    ).join('')
  },

  /**
   * Generate an optional value
   */
  option: <A>(gen: Generator<A>): Generator<O.Option<A>> => () =>
    Math.random() > 0.5 ? O.some(gen()) : O.none,

  /**
   * Generate a list of values
   */
  list: <A>(gen: Generator<A>, maxSize: number = 10): Generator<readonly A[]> =>
    () => {
      const size = Math.floor(Math.random() * maxSize)
      return Array.from({ length: size }, () => gen())
    },

  /**
   * Generate a LocationId
   */
  locationId: (): Generator<LocationId> => () =>
    LocationId.of(`Q${Math.floor(Math.random() * 1000000)}`),

  /**
   * Generate a Location
   */
  location: (): Generator<Location> => () =>
    createLocation(
      Gen.locationId()(),
      Gen.alphaNumStr(20)(),
      Gen.posInt(10000000)()
    ),

  /**
   * Generate an Attraction
   */
  attraction: (): Generator<Attraction> => () =>
    createAttraction(
      Gen.alphaNumStr(30)(),
      Gen.option(Gen.alphaNumStr(100))(),
      Gen.location()()
    ),

  /**
   * Generate a MusicArtist
   */
  musicArtist: (): Generator<MusicArtist> => () =>
    createMusicArtist(Gen.alphaNumStr(20)(), Gen.option(Gen.alphaNumStr(10))()),

  /**
   * Generate a Movie
   */
  movie: (): Generator<Movie> => () =>
    createMovie(
      Gen.alphaNumStr(30)(),
      Gen.option(() => Gen.int(1900, 2024)())()
    ),
}

/**
 * Run a property-based test
 */
export const forAll = <A>(
  gen: Generator<A>,
  property: (a: A) => boolean,
  iterations: number = 100
): boolean => {
  for (let i = 0; i < iterations; i++) {
    const value = gen()
    if (!property(value)) {
      return false
    }
  }
  return true
}

/**
 * Run a property-based test with two generators
 */
export const forAll2 = <A, B>(
  genA: Generator<A>,
  genB: Generator<B>,
  property: (a: A, b: B) => boolean,
  iterations: number = 100
): boolean => {
  for (let i = 0; i < iterations; i++) {
    const a = genA()
    const b = genB()
    if (!property(a, b)) {
      return false
    }
  }
  return true
}

// =============================================================================
// 12.2 Location Filtering Example
// =============================================================================

/**
 * Filter locations by minimum population
 */
export const filterPopularLocations = (
  locations: readonly Location[],
  minPopulation: number
): readonly Location[] => locations.filter((l) => l.population >= minPopulation)

/**
 * Sort locations by population descending
 */
export const sortLocationsByPopulation = (
  locations: readonly Location[]
): readonly Location[] =>
  [...locations].sort((a, b) => b.population - a.population)

/**
 * Get top N popular locations
 */
export const topPopularLocations = (
  locations: readonly Location[],
  n: number
): readonly Location[] => pipe(locations, sortLocationsByPopulation, A.takeLeft(n))

// =============================================================================
// 12.3 Validation Helpers
// =============================================================================

/**
 * Validate attraction name is not empty
 */
export const validateAttractionName = (
  name: string
): E.Either<string, string> =>
  name.trim().length > 0
    ? E.right(name.trim())
    : E.left('Attraction name cannot be empty')

/**
 * Validate limit is positive
 */
export const validateLimit = (limit: number): E.Either<string, number> =>
  limit > 0 ? E.right(limit) : E.left('Limit must be positive')

/**
 * Validate population is non-negative
 */
export const validatePopulation = (
  population: number
): E.Either<string, number> =>
  population >= 0
    ? E.right(population)
    : E.left('Population cannot be negative')

/**
 * Validate hotel rating is between 0 and 5
 */
export const validateRating = (rating: number): E.Either<string, number> =>
  rating >= 0 && rating <= 5
    ? E.right(rating)
    : E.left('Rating must be between 0 and 5')

// =============================================================================
// 12.4 Sample Data
// =============================================================================

export const sampleLocation1: Location = createLocation(
  'Q90',
  'Paris',
  2161000
)

export const sampleLocation2: Location = createLocation(
  'Q84',
  'London',
  8982000
)

export const sampleLocation3: Location = createLocation(
  'Q60',
  'New York City',
  8336817
)

export const sampleAttraction1: Attraction = createAttraction(
  'Eiffel Tower',
  O.some('Iconic iron lattice tower on the Champ de Mars'),
  sampleLocation1
)

export const sampleAttraction2: Attraction = createAttraction(
  'Big Ben',
  O.some('Famous clock tower at the north end of the Palace of Westminster'),
  sampleLocation2
)

export const sampleAttraction3: Attraction = createAttraction(
  'Statue of Liberty',
  O.some('Colossal neoclassical sculpture on Liberty Island'),
  sampleLocation3
)

export const sampleArtist1: MusicArtist = createMusicArtist(
  'Édith Piaf',
  O.some('Chanson')
)

export const sampleArtist2: MusicArtist = createMusicArtist(
  'The Beatles',
  O.some('Rock')
)

export const sampleArtist3: MusicArtist = createMusicArtist(
  'Jay-Z',
  O.some('Hip hop')
)

export const sampleMovie1: Movie = createMovie('Midnight in Paris', O.some(2011))
export const sampleMovie2: Movie = createMovie('Love Actually', O.some(2003))
export const sampleMovie3: Movie = createMovie('Home Alone 2', O.some(1992))

export const sampleHotel1: Hotel = createHotel(
  'Hôtel Plaza Athénée',
  4.8,
  sampleLocation1
)

export const sampleHotel2: Hotel = createHotel('The Ritz', 4.9, sampleLocation2)

export const sampleHotel3: Hotel = createHotel(
  'The Plaza',
  4.7,
  sampleLocation3
)

/**
 * Create a fully configured test DataAccess with sample data
 */
export const sampleDataAccess: DataAccess = createTestDataAccess({
  attractions: [sampleAttraction1, sampleAttraction2, sampleAttraction3],
  artists: [sampleArtist1, sampleArtist2, sampleArtist3],
  movies: [sampleMovie1, sampleMovie2, sampleMovie3],
  hotels: [sampleHotel1, sampleHotel2, sampleHotel3],
})
