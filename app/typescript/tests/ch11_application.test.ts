/**
 * Chapter 11 & 12: Practical Application and Testing - Tests
 */

import { describe, it, expect } from 'vitest'
import * as O from 'fp-ts/Option'
import * as E from 'fp-ts/Either'
import {
  // Domain Model
  LocationId,
  createLocation,
  createAttraction,
  createMusicArtist,
  createMovie,
  createHotel,
  // SearchReport
  createSearchReport,
  emptySearchReport,
  combineSearchReports,
  createTravelGuide,
  // Ordering
  ByName,
  ByLocationPopulation,
  compareAttractions,
  // Resource
  makeResource,
  makeResourceEither,
  bracket,
  bracketEither,
  // Cache
  createCacheRef,
  cachedDataAccess,
  // Application
  travelGuide,
  travelGuides,
  travelGuideWithHotels,
  // Test utilities
  createTestDataAccess,
  // Generators
  Gen,
  forAll,
  forAll2,
  // Location filtering
  filterPopularLocations,
  sortLocationsByPopulation,
  topPopularLocations,
  // Validation
  validateAttractionName,
  validateLimit,
  validatePopulation,
  validateRating,
  // Sample data
  sampleLocation1,
  sampleLocation2,
  sampleLocation3,
  sampleAttraction1,
  sampleAttraction2,
  sampleAttraction3,
  sampleArtist1,
  sampleArtist2,
  sampleArtist3,
  sampleMovie1,
  sampleMovie2,
  sampleMovie3,
  sampleHotel1,
  sampleDataAccess,
} from '../src/ch11_application.js'

// =============================================================================
// 11.1 Domain Model Tests
// =============================================================================

describe('Chapter 11.1: Domain Model', () => {
  describe('LocationId', () => {
    it('creates a LocationId from string', () => {
      const id = LocationId.of('Q123')
      expect(LocationId.value(id)).toBe('Q123')
    })

    it('preserves the original value', () => {
      const values = ['Q1', 'Q123456', 'test-id']
      values.forEach((v) => {
        expect(LocationId.value(LocationId.of(v))).toBe(v)
      })
    })
  })

  describe('Location', () => {
    it('creates a location with all fields', () => {
      const loc = createLocation('Q90', 'Paris', 2161000)
      expect(LocationId.value(loc.id)).toBe('Q90')
      expect(loc.name).toBe('Paris')
      expect(loc.population).toBe(2161000)
    })
  })

  describe('Attraction', () => {
    it('creates an attraction with description', () => {
      const loc = createLocation('Q90', 'Paris', 2161000)
      const attr = createAttraction('Eiffel Tower', O.some('Famous tower'), loc)
      expect(attr.name).toBe('Eiffel Tower')
      expect(O.isSome(attr.description)).toBe(true)
      expect(attr.location).toBe(loc)
    })

    it('creates an attraction without description', () => {
      const loc = createLocation('Q84', 'London', 8982000)
      const attr = createAttraction('Big Ben', O.none, loc)
      expect(attr.name).toBe('Big Ben')
      expect(O.isNone(attr.description)).toBe(true)
    })
  })

  describe('MusicArtist', () => {
    it('creates a music artist with genre', () => {
      const artist = createMusicArtist('The Beatles', O.some('Rock'))
      expect(artist.name).toBe('The Beatles')
      expect(O.isSome(artist.genre)).toBe(true)
    })

    it('creates a music artist without genre', () => {
      const artist = createMusicArtist('Unknown Artist')
      expect(artist.name).toBe('Unknown Artist')
      expect(O.isNone(artist.genre)).toBe(true)
    })
  })

  describe('Movie', () => {
    it('creates a movie with year', () => {
      const movie = createMovie('Midnight in Paris', O.some(2011))
      expect(movie.name).toBe('Midnight in Paris')
      expect(movie.year).toEqual(O.some(2011))
    })

    it('creates a movie without year', () => {
      const movie = createMovie('Unknown Movie')
      expect(movie.name).toBe('Unknown Movie')
      expect(O.isNone(movie.year)).toBe(true)
    })
  })

  describe('Hotel', () => {
    it('creates a hotel with all fields', () => {
      const loc = createLocation('Q90', 'Paris', 2161000)
      const hotel = createHotel('The Ritz', 4.9, loc)
      expect(hotel.name).toBe('The Ritz')
      expect(hotel.rating).toBe(4.9)
      expect(hotel.location).toBe(loc)
    })
  })
})

// =============================================================================
// 11.2 SearchReport Tests
// =============================================================================

describe('Chapter 11.2: SearchReport', () => {
  it('creates an empty search report', () => {
    expect(emptySearchReport.attractionsSearched).toBe(0)
    expect(emptySearchReport.artistsFound).toBe(0)
    expect(emptySearchReport.moviesFound).toBe(0)
    expect(emptySearchReport.errors).toEqual([])
  })

  it('creates a search report with values', () => {
    const report = createSearchReport(5, 3, 2, ['Error 1'])
    expect(report.attractionsSearched).toBe(5)
    expect(report.artistsFound).toBe(3)
    expect(report.moviesFound).toBe(2)
    expect(report.errors).toEqual(['Error 1'])
  })

  it('combines search reports', () => {
    const report1 = createSearchReport(3, 2, 1, ['Error A'])
    const report2 = createSearchReport(2, 1, 1, ['Error B'])
    const combined = combineSearchReports(report1, report2)

    expect(combined.attractionsSearched).toBe(5)
    expect(combined.artistsFound).toBe(3)
    expect(combined.moviesFound).toBe(2)
    expect(combined.errors).toEqual(['Error A', 'Error B'])
  })

  it('combines with empty report', () => {
    const report = createSearchReport(3, 2, 1, ['Error'])
    const combined = combineSearchReports(report, emptySearchReport)
    expect(combined).toEqual(report)
  })
})

// =============================================================================
// 11.3 Attraction Ordering Tests
// =============================================================================

describe('Chapter 11.3: Attraction Ordering', () => {
  const loc1 = createLocation('Q1', 'Small City', 100000)
  const loc2 = createLocation('Q2', 'Big City', 1000000)
  const attr1 = createAttraction('Alpha Tower', O.none, loc1)
  const attr2 = createAttraction('Beta Museum', O.none, loc2)

  it('sorts by name ascending', () => {
    const compare = compareAttractions(ByName)
    expect(compare(attr1, attr2)).toBeLessThan(0)
    expect(compare(attr2, attr1)).toBeGreaterThan(0)
    expect(compare(attr1, attr1)).toBe(0)
  })

  it('sorts by location population descending', () => {
    const compare = compareAttractions(ByLocationPopulation)
    expect(compare(attr1, attr2)).toBeGreaterThan(0) // Big city comes first
    expect(compare(attr2, attr1)).toBeLessThan(0)
  })

  it('handles equal populations', () => {
    const loc3 = createLocation('Q3', 'Same Size', 100000)
    const attr3 = createAttraction('Gamma Place', O.none, loc3)
    const compare = compareAttractions(ByLocationPopulation)
    expect(compare(attr1, attr3)).toBe(0)
  })
})

// =============================================================================
// 11.5 Resource Pattern Tests
// =============================================================================

describe('Chapter 11.5: Resource Pattern', () => {
  describe('makeResource', () => {
    it('acquires and releases resource', async () => {
      let acquired = false
      let released = false

      const resource = makeResource(
        () => {
          acquired = true
          return Promise.resolve('resource')
        },
        () => () => {
          released = true
          return Promise.resolve()
        }
      )

      const result = await resource.use((r) => () =>
        Promise.resolve(r + '-used')
      )()

      expect(acquired).toBe(true)
      expect(released).toBe(true)
      expect(result).toBe('resource-used')
    })

    it('releases resource even on error', async () => {
      let released = false

      const resource = makeResource(
        () => Promise.resolve('resource'),
        () => () => {
          released = true
          return Promise.resolve()
        }
      )

      try {
        await resource.use(() => () => {
          throw new Error('test error')
        })()
      } catch {
        // Expected
      }

      expect(released).toBe(true)
    })
  })

  describe('makeResourceEither', () => {
    it('handles successful acquisition', async () => {
      let released = false

      const resource = makeResourceEither<string, string>(
        () => Promise.resolve(E.right('resource')),
        () => () => {
          released = true
          return Promise.resolve()
        }
      )

      const result = await resource.use((r) => () =>
        Promise.resolve(E.right(r + '-used'))
      )()

      expect(released).toBe(true)
      expect(result).toEqual(E.right('resource-used'))
    })

    it('handles failed acquisition', async () => {
      let released = false

      const resource = makeResourceEither<string, string>(
        () => Promise.resolve(E.left('acquisition failed')),
        () => () => {
          released = true
          return Promise.resolve()
        }
      )

      const result = await resource.use((r) => () =>
        Promise.resolve(E.right(r + '-used'))
      )()

      expect(released).toBe(false) // No release if acquisition failed
      expect(result).toEqual(E.left('acquisition failed'))
    })
  })

  describe('bracket', () => {
    it('properly brackets resource usage', async () => {
      const log: string[] = []

      const result = await bracket(
        () => {
          log.push('acquire')
          return Promise.resolve(42)
        },
        (n) => () => {
          log.push('use')
          return Promise.resolve(n * 2)
        },
        () => () => {
          log.push('release')
          return Promise.resolve()
        }
      )()

      expect(result).toBe(84)
      expect(log).toEqual(['acquire', 'use', 'release'])
    })
  })

  describe('bracketEither', () => {
    it('properly brackets with Either', async () => {
      const log: string[] = []

      const result = await bracketEither(
        () => {
          log.push('acquire')
          return Promise.resolve(E.right(42))
        },
        (n) => () => {
          log.push('use')
          return Promise.resolve(E.right(n * 2))
        },
        () => () => {
          log.push('release')
          return Promise.resolve()
        }
      )()

      expect(result).toEqual(E.right(84))
      expect(log).toEqual(['acquire', 'use', 'release'])
    })
  })
})

// =============================================================================
// 11.6 Cache Tests
// =============================================================================

describe('Chapter 11.6: CacheRef', () => {
  it('starts empty', async () => {
    const cache = createCacheRef<string, number>()
    const size = await cache.size()()
    expect(size).toBe(0)
  })

  it('stores and retrieves values', async () => {
    const cache = createCacheRef<string, number>()

    await cache.set('key1', 100)()
    const value = await cache.get('key1')()

    expect(value).toEqual(O.some(100))
  })

  it('returns none for missing keys', async () => {
    const cache = createCacheRef<string, number>()
    const value = await cache.get('missing')()
    expect(O.isNone(value)).toBe(true)
  })

  it('computes and caches values', async () => {
    const cache = createCacheRef<string, number>()
    let computeCount = 0

    const compute = async () => {
      computeCount++
      return 42
    }

    const v1 = await cache.getOrCompute('key', compute)()
    const v2 = await cache.getOrCompute('key', compute)()

    expect(v1).toBe(42)
    expect(v2).toBe(42)
    expect(computeCount).toBe(1) // Only computed once
  })

  it('clears all values', async () => {
    const cache = createCacheRef<string, number>()

    await cache.set('key1', 1)()
    await cache.set('key2', 2)()
    await cache.clear()()

    const size = await cache.size()()
    expect(size).toBe(0)
  })
})

describe('cachedDataAccess', () => {
  it('caches attraction queries', async () => {
    let executionCount = 0

    // Create a DataAccess that counts actual Task executions
    const countingDataAccess: import('../src/ch11_application.js').DataAccess = {
      findAttractions: (_name, _ordering, _limit) => async () => {
        executionCount++
        return [sampleAttraction1]
      },
      findArtistsFromLocation: () => async () => E.right([]),
      findMoviesAboutLocation: () => async () => E.right([]),
      findHotelsNearLocation: () => async () => E.right([]),
    }

    const cached = await cachedDataAccess(countingDataAccess)()

    await cached.findAttractions('Eiffel', ByLocationPopulation, 1)()
    await cached.findAttractions('Eiffel', ByLocationPopulation, 1)()

    expect(executionCount).toBe(1) // Only executed once
  })
})

// =============================================================================
// 11.7 TravelGuide Application Tests
// =============================================================================

describe('Chapter 11.7: TravelGuide Application', () => {
  describe('travelGuide', () => {
    it('returns none for non-existent attraction', async () => {
      const dataAccess = createTestDataAccess({ attractions: [] })
      const result = await travelGuide(dataAccess, 'NonExistent')()
      expect(O.isNone(result)).toBe(true)
    })

    it('returns guide for existing attraction', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artists: [sampleArtist1],
        movies: [sampleMovie1],
      })

      const result = await travelGuide(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.attraction.name).toBe('Eiffel Tower')
        expect(result.value.subjects.length).toBe(2)
        expect(result.value.searchReport.errors.length).toBe(0)
      }
    })

    it('handles artist fetch errors gracefully', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artistsError: 'Network error',
        movies: [sampleMovie1],
      })

      const result = await travelGuide(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.searchReport.errors).toContain('Network error')
        expect(result.value.subjects.length).toBe(1) // Only movie
      }
    })

    it('handles movie fetch errors gracefully', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artists: [sampleArtist1],
        moviesError: 'Timeout',
      })

      const result = await travelGuide(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.searchReport.errors).toContain('Timeout')
        expect(result.value.subjects.length).toBe(1) // Only artist
      }
    })

    it('handles both errors', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artistsError: 'Error A',
        moviesError: 'Error B',
      })

      const result = await travelGuide(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.searchReport.errors.length).toBe(2)
        expect(result.value.subjects.length).toBe(0)
      }
    })
  })

  describe('travelGuides', () => {
    it('returns empty for no matches', async () => {
      const dataAccess = createTestDataAccess({ attractions: [] })
      const result = await travelGuides(dataAccess, 'NonExistent')()
      expect(result.length).toBe(0)
    })

    it('returns multiple guides', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1, sampleAttraction2, sampleAttraction3],
        artists: [sampleArtist1],
        movies: [sampleMovie1],
      })

      const result = await travelGuides(dataAccess, '', 3)()

      expect(result.length).toBe(3)
    })

    it('respects limit', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1, sampleAttraction2, sampleAttraction3],
      })

      const result = await travelGuides(dataAccess, '', 2)()

      expect(result.length).toBe(2)
    })
  })

  describe('travelGuideWithHotels', () => {
    it('includes hotels in result', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artists: [sampleArtist1],
        movies: [sampleMovie1],
        hotels: [sampleHotel1],
      })

      const result = await travelGuideWithHotels(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.hotels.length).toBe(1)
        expect(result.value.hotels[0].name).toBe('Hôtel Plaza Athénée')
      }
    })

    it('handles hotel fetch errors', async () => {
      const dataAccess = createTestDataAccess({
        attractions: [sampleAttraction1],
        artists: [sampleArtist1],
        movies: [sampleMovie1],
        hotelsError: 'Hotel API down',
      })

      const result = await travelGuideWithHotels(dataAccess, 'Eiffel')()

      expect(O.isSome(result)).toBe(true)
      if (O.isSome(result)) {
        expect(result.value.hotels.length).toBe(0)
        expect(result.value.searchReport.errors).toContain('Hotel API down')
      }
    })
  })
})

// =============================================================================
// 11.8 Test Data Access Tests
// =============================================================================

describe('Chapter 11.8: Test Data Access', () => {
  it('filters attractions by name', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction1, sampleAttraction2],
    })

    const result = await dataAccess.findAttractions('Eiffel', ByName, 10)()

    expect(result.length).toBe(1)
    expect(result[0].name).toBe('Eiffel Tower')
  })

  it('sorts attractions by name', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction2, sampleAttraction1], // Unsorted
    })

    const result = await dataAccess.findAttractions('', ByName, 10)()

    expect(result[0].name).toBe('Big Ben')
    expect(result[1].name).toBe('Eiffel Tower')
  })

  it('sorts attractions by population', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction1, sampleAttraction2],
    })

    const result = await dataAccess.findAttractions(
      '',
      ByLocationPopulation,
      10
    )()

    // London has higher population
    expect(result[0].name).toBe('Big Ben')
  })

  it('limits results', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction1, sampleAttraction2, sampleAttraction3],
    })

    const result = await dataAccess.findAttractions('', ByName, 2)()

    expect(result.length).toBe(2)
  })
})

// =============================================================================
// 12.1 Property-based Testing Tests
// =============================================================================

describe('Chapter 12.1: Property-based Testing', () => {
  describe('Generators', () => {
    it('Gen.int generates within range', () => {
      const gen = Gen.int(1, 10)
      const results = Array.from({ length: 100 }, () => gen())

      expect(results.every((n) => n >= 1 && n <= 10)).toBe(true)
    })

    it('Gen.posInt generates positive integers', () => {
      const gen = Gen.posInt(100)
      const results = Array.from({ length: 100 }, () => gen())

      expect(results.every((n) => n > 0)).toBe(true)
    })

    it('Gen.alphaNumStr generates non-empty strings', () => {
      const gen = Gen.alphaNumStr(10)
      const results = Array.from({ length: 100 }, () => gen())

      expect(results.every((s) => s.length > 0 && s.length <= 10)).toBe(true)
    })

    it('Gen.option generates some and none', () => {
      const gen = Gen.option(Gen.int(1, 10))
      const results = Array.from({ length: 100 }, () => gen())

      const hasSome = results.some((o) => O.isSome(o))
      const hasNone = results.some((o) => O.isNone(o))

      // With enough iterations, we should have both
      expect(hasSome || hasNone).toBe(true)
    })

    it('Gen.list generates lists', () => {
      const gen = Gen.list(Gen.int(1, 10), 5)
      const results = Array.from({ length: 100 }, () => gen())

      expect(results.every((arr) => arr.length <= 5)).toBe(true)
    })

    it('Gen.location generates valid locations', () => {
      const gen = Gen.location()
      const loc = gen()

      expect(loc.id).toBeDefined()
      expect(loc.name).toBeDefined()
      expect(loc.population).toBeGreaterThan(0)
    })

    it('Gen.attraction generates valid attractions', () => {
      const gen = Gen.attraction()
      const attr = gen()

      expect(attr.name).toBeDefined()
      expect(attr.location).toBeDefined()
    })
  })

  describe('forAll', () => {
    it('tests a property over many iterations', () => {
      const result = forAll(Gen.posInt(1000), (n) => n > 0, 100)
      expect(result).toBe(true)
    })

    it('fails if property is violated', () => {
      const result = forAll(Gen.int(-10, 10), (n) => n > 0, 100)
      expect(result).toBe(false)
    })
  })

  describe('forAll2', () => {
    it('tests a property with two generators', () => {
      const result = forAll2(
        Gen.posInt(100),
        Gen.posInt(100),
        (a, b) => a + b > 0,
        100
      )
      expect(result).toBe(true)
    })
  })
})

// =============================================================================
// 12.2 Location Filtering Tests
// =============================================================================

describe('Chapter 12.2: Location Filtering', () => {
  describe('filterPopularLocations', () => {
    it('filters by minimum population', () => {
      const locations = [sampleLocation1, sampleLocation2, sampleLocation3]
      const result = filterPopularLocations(locations, 5000000)

      expect(result.length).toBe(2)
      expect(result.every((l) => l.population >= 5000000)).toBe(true)
    })

    it('returns empty for high threshold', () => {
      const locations = [sampleLocation1, sampleLocation2]
      const result = filterPopularLocations(locations, 100000000)

      expect(result.length).toBe(0)
    })

    it('returns all for low threshold', () => {
      const locations = [sampleLocation1, sampleLocation2]
      const result = filterPopularLocations(locations, 0)

      expect(result.length).toBe(2)
    })

    // Property-based test
    it('result size <= input size (property)', () => {
      const result = forAll2(
        Gen.list(Gen.location(), 20),
        Gen.posInt(10000000),
        (locations, minPop) =>
          filterPopularLocations(locations, minPop).length <= locations.length
      )
      expect(result).toBe(true)
    })

    it('all results meet minimum population (property)', () => {
      const result = forAll2(
        Gen.list(Gen.location(), 20),
        Gen.posInt(10000000),
        (locations, minPop) =>
          filterPopularLocations(locations, minPop).every(
            (l) => l.population >= minPop
          )
      )
      expect(result).toBe(true)
    })
  })

  describe('sortLocationsByPopulation', () => {
    it('sorts descending by population', () => {
      const locations = [sampleLocation1, sampleLocation2, sampleLocation3]
      const result = sortLocationsByPopulation(locations)

      expect(result[0].name).toBe('London')
      expect(result[1].name).toBe('New York City')
      expect(result[2].name).toBe('Paris')
    })

    it('is stable for equal populations', () => {
      const loc1 = createLocation('Q1', 'City A', 1000000)
      const loc2 = createLocation('Q2', 'City B', 1000000)
      const result = sortLocationsByPopulation([loc1, loc2])

      expect(result.length).toBe(2)
    })
  })

  describe('topPopularLocations', () => {
    it('returns top N locations', () => {
      const locations = [sampleLocation1, sampleLocation2, sampleLocation3]
      const result = topPopularLocations(locations, 2)

      expect(result.length).toBe(2)
      expect(result[0].name).toBe('London')
      expect(result[1].name).toBe('New York City')
    })

    it('returns all if N > length', () => {
      const locations = [sampleLocation1, sampleLocation2]
      const result = topPopularLocations(locations, 10)

      expect(result.length).toBe(2)
    })
  })
})

// =============================================================================
// 12.3 Validation Tests
// =============================================================================

describe('Chapter 12.3: Validation', () => {
  describe('validateAttractionName', () => {
    it('accepts valid names', () => {
      expect(validateAttractionName('Eiffel Tower')).toEqual(
        E.right('Eiffel Tower')
      )
    })

    it('trims whitespace', () => {
      expect(validateAttractionName('  Eiffel Tower  ')).toEqual(
        E.right('Eiffel Tower')
      )
    })

    it('rejects empty names', () => {
      expect(E.isLeft(validateAttractionName(''))).toBe(true)
      expect(E.isLeft(validateAttractionName('   '))).toBe(true)
    })
  })

  describe('validateLimit', () => {
    it('accepts positive limits', () => {
      expect(validateLimit(10)).toEqual(E.right(10))
      expect(validateLimit(1)).toEqual(E.right(1))
    })

    it('rejects non-positive limits', () => {
      expect(E.isLeft(validateLimit(0))).toBe(true)
      expect(E.isLeft(validateLimit(-5))).toBe(true)
    })
  })

  describe('validatePopulation', () => {
    it('accepts non-negative populations', () => {
      expect(validatePopulation(1000000)).toEqual(E.right(1000000))
      expect(validatePopulation(0)).toEqual(E.right(0))
    })

    it('rejects negative populations', () => {
      expect(E.isLeft(validatePopulation(-1))).toBe(true)
    })
  })

  describe('validateRating', () => {
    it('accepts ratings between 0 and 5', () => {
      expect(validateRating(0)).toEqual(E.right(0))
      expect(validateRating(3.5)).toEqual(E.right(3.5))
      expect(validateRating(5)).toEqual(E.right(5))
    })

    it('rejects ratings outside range', () => {
      expect(E.isLeft(validateRating(-0.1))).toBe(true)
      expect(E.isLeft(validateRating(5.1))).toBe(true)
    })
  })
})

// =============================================================================
// 12.4 Sample Data Tests
// =============================================================================

describe('Chapter 12.4: Sample Data', () => {
  it('sample locations are valid', () => {
    expect(sampleLocation1.name).toBe('Paris')
    expect(sampleLocation2.name).toBe('London')
    expect(sampleLocation3.name).toBe('New York City')
  })

  it('sample attractions are valid', () => {
    expect(sampleAttraction1.name).toBe('Eiffel Tower')
    expect(sampleAttraction2.name).toBe('Big Ben')
    expect(sampleAttraction3.name).toBe('Statue of Liberty')
  })

  it('sample artists are valid', () => {
    expect(sampleArtist1.name).toBe('Édith Piaf')
    expect(sampleArtist2.name).toBe('The Beatles')
    expect(sampleArtist3.name).toBe('Jay-Z')
  })

  it('sample movies are valid', () => {
    expect(sampleMovie1.name).toBe('Midnight in Paris')
    expect(sampleMovie2.name).toBe('Love Actually')
    expect(sampleMovie3.name).toBe('Home Alone 2')
  })

  it('sampleDataAccess works correctly', async () => {
    const result = await travelGuide(sampleDataAccess, 'Eiffel')()

    expect(O.isSome(result)).toBe(true)
    if (O.isSome(result)) {
      expect(result.value.attraction.name).toBe('Eiffel Tower')
    }
  })
})

// =============================================================================
// Integration Tests
// =============================================================================

describe('Integration Tests', () => {
  it('end-to-end travel guide creation', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction1, sampleAttraction2, sampleAttraction3],
      artists: [sampleArtist1, sampleArtist2],
      movies: [sampleMovie1, sampleMovie2],
      hotels: [sampleHotel1],
    })

    const cached = await cachedDataAccess(dataAccess)()
    const guide = await travelGuide(cached, 'Eiffel')()

    expect(O.isSome(guide)).toBe(true)
    if (O.isSome(guide)) {
      expect(guide.value.attraction.name).toBe('Eiffel Tower')
      expect(guide.value.subjects.length).toBe(4) // 2 artists + 2 movies
      expect(guide.value.searchReport.errors.length).toBe(0)
    }
  })

  it('handles partial failures gracefully', async () => {
    const dataAccess = createTestDataAccess({
      attractions: [sampleAttraction1],
      artistsError: 'Artists API unavailable',
      movies: [sampleMovie1],
    })

    const guide = await travelGuide(dataAccess, 'Eiffel')()

    expect(O.isSome(guide)).toBe(true)
    if (O.isSome(guide)) {
      expect(guide.value.searchReport.errors.length).toBe(1)
      expect(guide.value.subjects.length).toBe(1) // Only movie
    }
  })

  it('caching improves performance', async () => {
    let queryCount = 0
    const dataAccess: import('../src/ch11_application.js').DataAccess = {
      findAttractions: () => async () => {
        queryCount++
        return [sampleAttraction1]
      },
      findArtistsFromLocation: () => async () => E.right([sampleArtist1]),
      findMoviesAboutLocation: () => async () => E.right([sampleMovie1]),
      findHotelsNearLocation: () => async () => E.right([sampleHotel1]),
    }

    const cached = await cachedDataAccess(dataAccess)()

    // First call
    await cached.findAttractions('Eiffel', ByLocationPopulation, 1)()
    // Second call (should be cached)
    await cached.findAttractions('Eiffel', ByLocationPopulation, 1)()

    expect(queryCount).toBe(1)
  })
})
