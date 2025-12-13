# frozen_string_literal: true

require_relative 'ch08_io'
require_relative 'ch11_application'

# Chapter 12: Testing Strategies for Functional Programming
# 関数型プログラミングのテスト戦略
module Ch12Testing
  # ===========================================================================
  # 12.1 Test Helpers
  # ===========================================================================

  # Create a test stub for DataAccess
  # DataAccess のテストスタブを作成
  class TestDataAccess
    include Ch11Application::DataAccess

    def initialize(
      attractions: [],
      artists_by_location: {},
      movies_by_location: {},
      hotels_by_location: {},
      artist_errors: {},
      movie_errors: {},
      hotel_errors: {}
    )
      @attractions = attractions
      @artists_by_location = artists_by_location
      @movies_by_location = movies_by_location
      @hotels_by_location = hotels_by_location
      @artist_errors = artist_errors
      @movie_errors = movie_errors
      @hotel_errors = hotel_errors
    end

    def find_attractions(name, ordering, limit)
      Ch08IO::IO.delay do
        filtered = @attractions.select { |a| a.name.downcase.include?(name.downcase) }
        sorted = case ordering
                 when Ch11Application::AttractionOrdering::BY_NAME
                   filtered.sort_by(&:name)
                 when Ch11Application::AttractionOrdering::BY_LOCATION_POPULATION
                   filtered.sort_by { |a| -a.location.population }
                 else
                   filtered
                 end
        sorted.take(limit)
      end
    end

    def find_artists_from_location(location_id, limit)
      Ch08IO::IO.delay do
        if @artist_errors[location_id]
          { success: false, error: @artist_errors[location_id] }
        else
          artists = @artists_by_location[location_id] || []
          { success: true, value: artists.take(limit) }
        end
      end
    end

    def find_movies_about_location(location_id, limit)
      Ch08IO::IO.delay do
        if @movie_errors[location_id]
          { success: false, error: @movie_errors[location_id] }
        else
          movies = @movies_by_location[location_id] || []
          { success: true, value: movies.take(limit) }
        end
      end
    end

    def find_hotels_near_location(location_id, limit)
      Ch08IO::IO.delay do
        if @hotel_errors[location_id]
          { success: false, error: @hotel_errors[location_id] }
        else
          hotels = @hotels_by_location[location_id] || []
          { success: true, value: hotels.take(limit) }
        end
      end
    end
  end

  # ===========================================================================
  # 12.2 Test Data Builders
  # ===========================================================================

  # Builder for Location
  # Location のビルダー
  class LocationBuilder
    def initialize
      @id = Ch11Application::LocationId.new("Q#{rand(1000)}")
      @name = 'Test City'
      @population = 100_000
    end

    def with_id(id)
      @id = id.is_a?(Ch11Application::LocationId) ? id : Ch11Application::LocationId.new(id)
      self
    end

    def with_name(name)
      @name = name
      self
    end

    def with_population(population)
      @population = population
      self
    end

    def build
      Ch11Application::Location.new(id: @id, name: @name, population: @population)
    end
  end

  # Builder for Attraction
  # Attraction のビルダー
  class AttractionBuilder
    def initialize
      @name = 'Test Attraction'
      @description = 'A test attraction'
      @location = LocationBuilder.new.build
    end

    def with_name(name)
      @name = name
      self
    end

    def with_description(description)
      @description = description
      self
    end

    def with_location(location)
      @location = location
      self
    end

    def build
      Ch11Application::Attraction.new(
        name: @name,
        description: @description,
        location: @location
      )
    end
  end

  # ===========================================================================
  # 12.3 Property-Based Testing Helpers
  # ===========================================================================

  # Generator for random data
  # ランダムデータのジェネレータ
  module Gen
    def self.string(length = 10)
      chars = ('a'..'z').to_a + ('A'..'Z').to_a
      Array.new(length) { chars.sample }.join
    end

    def self.int(min = 0, max = 100)
      rand(min..max)
    end

    def self.positive_int(max = 1_000_000)
      rand(1..max)
    end

    def self.boolean
      [true, false].sample
    end

    def self.one_of(*options)
      options.sample
    end

    def self.list_of(size = 5, &generator)
      Array.new(size) { generator.call }
    end

    def self.option(&generator)
      boolean ? generator.call : nil
    end

    def self.location_id
      Ch11Application::LocationId.new("Q#{positive_int(10_000)}")
    end

    def self.location
      Ch11Application::Location.new(
        id: location_id,
        name: string(8),
        population: positive_int(50_000_000)
      )
    end

    def self.attraction
      Ch11Application::Attraction.new(
        name: string(15),
        description: option { string(50) },
        location: location
      )
    end

    def self.music_artist
      Ch11Application::MusicArtist.new(
        name: string(10),
        genre: one_of('Rock', 'Pop', 'Jazz', 'Classical', 'Electronic')
      )
    end

    def self.movie
      Ch11Application::Movie.new(
        name: string(20),
        year: int(1900, 2024)
      )
    end

    def self.hotel
      Ch11Application::Hotel.new(
        name: string(15),
        rating: (int(30, 50) / 10.0).round(1),
        location: location
      )
    end
  end

  # ===========================================================================
  # 12.4 Property Testing Framework
  # ===========================================================================

  # Run property-based tests
  # プロパティベーステストを実行
  class PropertyTest
    attr_reader :name, :failures

    def initialize(name, iterations: 100)
      @name = name
      @iterations = iterations
      @failures = []
    end

    def for_all(*generators, &property)
      @iterations.times do |i|
        inputs = generators.map(&:call)
        begin
          result = property.call(*inputs)
          unless result
            @failures << { iteration: i, inputs: inputs, error: 'Property returned false' }
          end
        rescue StandardError => e
          @failures << { iteration: i, inputs: inputs, error: e.message }
        end
      end
      self
    end

    def passed?
      @failures.empty?
    end

    def summary
      if passed?
        "#{@name}: PASSED (#{@iterations} iterations)"
      else
        "#{@name}: FAILED (#{@failures.size}/#{@iterations} failures)"
      end
    end
  end

  # ===========================================================================
  # 12.5 Common Properties for Testing
  # ===========================================================================

  # Properties module contains reusable test properties
  # Properties モジュールは再利用可能なテストプロパティを含む
  module Properties
    # Filter results are bounded by input
    # フィルタ結果は入力で制限される
    def self.filter_bounded?(input, result)
      result.size <= input.size
    end

    # All filtered results satisfy predicate
    # すべてのフィルタ結果が述語を満たす
    def self.filter_satisfies?(result, &predicate)
      result.all?(&predicate)
    end

    # Limit is respected
    # 制限が守られている
    def self.limit_respected?(result, limit)
      result.size <= limit
    end

    # Ordering is preserved
    # 順序が保持されている
    def self.ordering_preserved?(result, &compare)
      result.each_cons(2).all? { |a, b| compare.call(a, b) }
    end

    # SearchReport errors are bounded
    # SearchReport のエラーが制限されている
    def self.errors_bounded?(search_report, max_errors)
      search_report.errors.size <= max_errors
    end
  end

  # ===========================================================================
  # 12.6 Pure Function Testing Examples
  # ===========================================================================

  # Filter locations by population (pure function to test)
  # 人口でロケーションをフィルタリング（テストする純粋関数）
  def self.filter_popular_locations(locations, min_population)
    locations.select { |loc| loc.population >= min_population }
  end

  # Sort attractions by name (pure function to test)
  # 名前でアトラクションをソート（テストする純粋関数）
  def self.sort_attractions_by_name(attractions)
    attractions.sort_by(&:name)
  end

  # Calculate average rating (pure function to test)
  # 平均レーティングを計算（テストする純粋関数）
  def self.average_rating(hotels)
    return nil if hotels.empty?

    hotels.sum(&:rating) / hotels.size.to_f
  end

  # Group artists by genre (pure function to test)
  # ジャンルでアーティストをグループ化（テストする純粋関数）
  def self.group_by_genre(artists)
    artists.group_by(&:genre)
  end

  # ===========================================================================
  # 12.7 Integration Test Helpers
  # ===========================================================================

  # Create a complete test scenario
  # 完全なテストシナリオを作成
  def self.create_test_scenario(options = {})
    location = options[:location] || LocationBuilder.new.with_population(1_000_000).build
    attraction = options[:attraction] || AttractionBuilder.new.with_location(location).build
    artists = options[:artists] || [Gen.music_artist, Gen.music_artist]
    movies = options[:movies] || [Gen.movie, Gen.movie]
    hotels = options[:hotels] || [Gen.hotel, Gen.hotel]

    TestDataAccess.new(
      attractions: [attraction],
      artists_by_location: { location.id => artists },
      movies_by_location: { location.id => movies },
      hotels_by_location: { location.id => hotels }
    )
  end

  # Create a failing test scenario
  # 失敗するテストシナリオを作成
  def self.create_failing_scenario(options = {})
    location = options[:location] || LocationBuilder.new.build
    attraction = options[:attraction] || AttractionBuilder.new.with_location(location).build

    TestDataAccess.new(
      attractions: [attraction],
      artists_by_location: {},
      movies_by_location: {},
      hotels_by_location: {},
      artist_errors: options[:artist_error] ? { location.id => options[:artist_error] } : {},
      movie_errors: options[:movie_error] ? { location.id => options[:movie_error] } : {},
      hotel_errors: options[:hotel_error] ? { location.id => options[:hotel_error] } : {}
    )
  end

  # ===========================================================================
  # 12.8 Assertion Helpers
  # ===========================================================================

  module Assertions
    def self.assert_travel_guide_valid(guide)
      raise 'Guide is nil' if guide.nil?
      raise 'Attraction is nil' if guide.attraction.nil?
      raise 'Search report is nil' if guide.search_report.nil?

      true
    end

    def self.assert_no_errors(guide)
      errors = guide.search_report.errors
      raise "Expected no errors but got: #{errors}" unless errors.empty?

      true
    end

    def self.assert_has_errors(guide, count: nil)
      errors = guide.search_report.errors
      raise 'Expected errors but got none' if errors.empty?
      raise "Expected #{count} errors but got #{errors.size}" if count && errors.size != count

      true
    end

    def self.assert_subjects_present(guide)
      raise 'Expected subjects but got none' if guide.subjects.empty?

      true
    end
  end

  # ===========================================================================
  # 12.9 Test Fixtures
  # ===========================================================================

  # Pre-built test fixtures
  # 事前に構築されたテストフィクスチャ
  module Fixtures
    def self.sydney
      @sydney ||= begin
        id = Ch11Application::LocationId.new('Q60')
        Ch11Application::Location.new(id: id, name: 'Sydney', population: 5_312_000)
      end
    end

    def self.sydney_opera_house
      @sydney_opera_house ||= Ch11Application::Attraction.new(
        name: 'Sydney Opera House',
        description: 'Iconic performing arts venue',
        location: sydney
      )
    end

    def self.sydney_artists
      @sydney_artists ||= [
        Ch11Application::MusicArtist.new(name: 'AC/DC', genre: 'Rock'),
        Ch11Application::MusicArtist.new(name: 'Sia', genre: 'Pop')
      ]
    end

    def self.sydney_movies
      @sydney_movies ||= [
        Ch11Application::Movie.new(name: 'Finding Nemo', year: 2003),
        Ch11Application::Movie.new(name: 'The Matrix', year: 1999)
      ]
    end

    def self.sydney_hotels
      @sydney_hotels ||= [
        Ch11Application::Hotel.new(name: 'Park Hyatt Sydney', rating: 4.8, location: sydney)
      ]
    end

    def self.complete_data_access
      @complete_data_access ||= TestDataAccess.new(
        attractions: [sydney_opera_house],
        artists_by_location: { sydney.id => sydney_artists },
        movies_by_location: { sydney.id => sydney_movies },
        hotels_by_location: { sydney.id => sydney_hotels }
      )
    end
  end

  # ===========================================================================
  # 12.10 Test Runner
  # ===========================================================================

  # Simple test runner for property tests
  # プロパティテスト用のシンプルなテストランナー
  class TestRunner
    def initialize
      @tests = []
    end

    def add_property(name, iterations: 100, &block)
      test = PropertyTest.new(name, iterations: iterations)
      block.call(test)
      @tests << test
    end

    def run_all
      results = @tests.map do |test|
        {
          name: test.name,
          passed: test.passed?,
          failures: test.failures,
          summary: test.summary
        }
      end

      {
        total: @tests.size,
        passed: results.count { |r| r[:passed] },
        failed: results.count { |r| !r[:passed] },
        results: results
      }
    end
  end
end
