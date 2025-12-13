# frozen_string_literal: true

require_relative 'ch08_io'
require_relative 'ch10_concurrency'

# Chapter 11: Practical Application Building
# 実践的なアプリケーション構築
module Ch11Application
  # ===========================================================================
  # 11.1 Domain Models
  # ===========================================================================

  # Value object for location ID
  # ロケーション ID の値オブジェクト
  class LocationId
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def ==(other)
      other.is_a?(LocationId) && @value == other.value
    end

    def eql?(other)
      self == other
    end

    def hash
      @value.hash
    end

    def to_s
      @value
    end
  end

  # Location (city, country, etc.)
  # ロケーション（都市、国など）
  Location = Struct.new(:id, :name, :population, keyword_init: true)

  # Attraction (tourist spot)
  # アトラクション（観光地）
  Attraction = Struct.new(:name, :description, :location, keyword_init: true)

  # Music artist
  # 音楽アーティスト
  MusicArtist = Struct.new(:name, :genre, keyword_init: true)

  # Movie
  # 映画
  Movie = Struct.new(:name, :year, keyword_init: true)

  # Hotel
  # ホテル
  Hotel = Struct.new(:name, :rating, :location, keyword_init: true)

  # Travel guide result
  # 旅行ガイドの結果
  TravelGuide = Struct.new(:attraction, :subjects, :search_report, keyword_init: true)

  # Search report for observability
  # 可観測性のための検索レポート
  SearchReport = Struct.new(:attractions_searched, :errors, keyword_init: true) do
    def self.empty
      new(attractions_searched: 0, errors: [])
    end

    def add_error(error)
      SearchReport.new(
        attractions_searched: attractions_searched,
        errors: errors + [error]
      )
    end

    def with_attractions_count(count)
      SearchReport.new(
        attractions_searched: count,
        errors: errors
      )
    end
  end

  # ===========================================================================
  # 11.2 Attraction Ordering
  # ===========================================================================

  module AttractionOrdering
    BY_NAME = :by_name
    BY_LOCATION_POPULATION = :by_location_population
  end

  # ===========================================================================
  # 11.3 DataAccess Interface (Abstract)
  # ===========================================================================

  # DataAccess module defines the interface for data access
  # DataAccess モジュールはデータアクセスのインターフェースを定義
  module DataAccess
    def find_attractions(_name, _ordering, _limit)
      raise NotImplementedError
    end

    def find_artists_from_location(_location_id, _limit)
      raise NotImplementedError
    end

    def find_movies_about_location(_location_id, _limit)
      raise NotImplementedError
    end

    def find_hotels_near_location(_location_id, _limit)
      raise NotImplementedError
    end
  end

  # ===========================================================================
  # 11.4 In-Memory DataAccess Implementation
  # ===========================================================================

  # Simple in-memory data access for testing and examples
  # テストと例のためのシンプルなインメモリデータアクセス
  class InMemoryDataAccess
    include DataAccess

    def initialize(attractions: [], artists: {}, movies: {}, hotels: {})
      @attractions = attractions
      @artists = artists # { LocationId => [MusicArtist] }
      @movies = movies   # { LocationId => [Movie] }
      @hotels = hotels   # { LocationId => [Hotel] }
    end

    def find_attractions(name, ordering, limit)
      Ch08IO::IO.delay do
        filtered = @attractions.select { |a| a.name.downcase.include?(name.downcase) }
        sorted = case ordering
                 when AttractionOrdering::BY_NAME
                   filtered.sort_by(&:name)
                 when AttractionOrdering::BY_LOCATION_POPULATION
                   filtered.sort_by { |a| -a.location.population }
                 else
                   filtered
                 end
        sorted.take(limit)
      end
    end

    def find_artists_from_location(location_id, limit)
      Ch08IO::IO.delay do
        artists = @artists[location_id] || []
        { success: true, value: artists.take(limit) }
      end
    end

    def find_movies_about_location(location_id, limit)
      Ch08IO::IO.delay do
        movies = @movies[location_id] || []
        { success: true, value: movies.take(limit) }
      end
    end

    def find_hotels_near_location(location_id, limit)
      Ch08IO::IO.delay do
        hotels = @hotels[location_id] || []
        { success: true, value: hotels.take(limit) }
      end
    end
  end

  # ===========================================================================
  # 11.5 Failing DataAccess (for testing error handling)
  # ===========================================================================

  # Data access that can simulate failures
  # 失敗をシミュレートできるデータアクセス
  class FailingDataAccess
    include DataAccess

    def initialize(base_access, failing_methods: [])
      @base_access = base_access
      @failing_methods = failing_methods
    end

    def find_attractions(name, ordering, limit)
      if @failing_methods.include?(:find_attractions)
        Ch08IO::IO.delay { raise 'Network error: find_attractions' }
      else
        @base_access.find_attractions(name, ordering, limit)
      end
    end

    def find_artists_from_location(location_id, limit)
      if @failing_methods.include?(:find_artists_from_location)
        Ch08IO::IO.pure({ success: false, error: 'Network error: artists' })
      else
        @base_access.find_artists_from_location(location_id, limit)
      end
    end

    def find_movies_about_location(location_id, limit)
      if @failing_methods.include?(:find_movies_about_location)
        Ch08IO::IO.pure({ success: false, error: 'Timeout: movies' })
      else
        @base_access.find_movies_about_location(location_id, limit)
      end
    end

    def find_hotels_near_location(location_id, limit)
      if @failing_methods.include?(:find_hotels_near_location)
        Ch08IO::IO.pure({ success: false, error: 'Service unavailable: hotels' })
      else
        @base_access.find_hotels_near_location(location_id, limit)
      end
    end
  end

  # ===========================================================================
  # 11.6 Cached DataAccess
  # ===========================================================================

  # Data access with caching using Ref
  # Ref を使ったキャッシュ付きデータアクセス
  class CachedDataAccess
    include DataAccess

    def initialize(base_access)
      @base_access = base_access
      @attractions_cache = Ch10Concurrency::Ref.new({})
      @artists_cache = Ch10Concurrency::Ref.new({})
      @movies_cache = Ch10Concurrency::Ref.new({})
      @hotels_cache = Ch10Concurrency::Ref.new({})
    end

    def find_attractions(name, ordering, limit)
      cache_key = "#{name}-#{ordering}-#{limit}"
      get_or_fetch(@attractions_cache, cache_key) do
        @base_access.find_attractions(name, ordering, limit)
      end
    end

    def find_artists_from_location(location_id, limit)
      cache_key = "#{location_id.value}-#{limit}"
      get_or_fetch(@artists_cache, cache_key) do
        @base_access.find_artists_from_location(location_id, limit)
      end
    end

    def find_movies_about_location(location_id, limit)
      cache_key = "#{location_id.value}-#{limit}"
      get_or_fetch(@movies_cache, cache_key) do
        @base_access.find_movies_about_location(location_id, limit)
      end
    end

    def find_hotels_near_location(location_id, limit)
      cache_key = "#{location_id.value}-#{limit}"
      get_or_fetch(@hotels_cache, cache_key) do
        @base_access.find_hotels_near_location(location_id, limit)
      end
    end

    def cache_stats
      {
        attractions: @attractions_cache.get.run!.size,
        artists: @artists_cache.get.run!.size,
        movies: @movies_cache.get.run!.size,
        hotels: @hotels_cache.get.run!.size
      }
    end

    private

    def get_or_fetch(cache_ref, key)
      Ch08IO::IO.delay do
        cached = cache_ref.get.run![key]
        if cached
          cached
        else
          result = yield.run!
          cache_ref.update { |c| c.merge(key => result) }.run!
          result
        end
      end
    end
  end

  # ===========================================================================
  # 11.7 Resource Management
  # ===========================================================================

  # Resource wraps a value with acquisition and release
  # Resource は取得と解放を伴う値をラップ
  class Resource
    def initialize(&acquire_release)
      @acquire_release = acquire_release
    end

    # Create a resource with acquire and release actions
    # 取得と解放のアクションでリソースを作成
    def self.make(acquire_io, &release)
      new do |&use_fn|
        Ch08IO::IO.delay do
          resource = acquire_io.run!
          begin
            use_fn.call(resource)
          ensure
            release.call(resource)
          end
        end
      end
    end

    # Use the resource
    # リソースを使用
    def use(&fn)
      @acquire_release.call(&fn)
    end

    # Map over the resource
    # リソースをマップ
    def fmap(&fn)
      Resource.new do |&use_fn|
        use do |resource|
          use_fn.call(fn.call(resource))
        end
      end
    end

    # FlatMap over resources
    # リソースを flatMap
    def bind(&fn)
      Resource.new do |&use_fn|
        use do |resource|
          fn.call(resource).use(&use_fn)
        end
      end
    end
  end

  # ===========================================================================
  # 11.8 File Resource Example
  # ===========================================================================

  # Create a file resource
  # ファイルリソースを作成
  def self.file_resource(path)
    Resource.make(
      Ch08IO::IO.delay { File.open(path, 'r') }
    ) { |file| file.close }
  end

  # Read all lines from a file
  # ファイルから全行を読み取り
  def self.read_lines(path)
    file_resource(path).use do |file|
      file.readlines.map(&:chomp)
    end
  end

  # ===========================================================================
  # 11.9 TravelGuide Application Logic
  # ===========================================================================

  # Build a travel guide for an attraction
  # アトラクションの旅行ガイドを構築
  def self.travel_guide(data_access, attraction_name)
    data_access.find_attractions(
      attraction_name,
      AttractionOrdering::BY_LOCATION_POPULATION,
      3
    ).bind do |attractions|
      if attractions.empty?
        Ch08IO::IO.pure(nil)
      else
        build_guides(data_access, attractions).fmap do |guides|
          guides.first
        end
      end
    end
  end

  # Build guides for multiple attractions
  # 複数のアトラクションのガイドを構築
  def self.build_guides(data_access, attractions)
    Ch08IO::IO.delay do
      attractions.map do |attraction|
        build_single_guide(data_access, attraction, attractions.size).run!
      end
    end
  end

  # Build a guide for a single attraction
  # 単一のアトラクションのガイドを構築
  def self.build_single_guide(data_access, attraction, total_attractions)
    artists_result = data_access.find_artists_from_location(
      attraction.location.id, 2
    ).run!

    movies_result = data_access.find_movies_about_location(
      attraction.location.id, 2
    ).run!

    errors = []
    artists = if artists_result[:success]
                artists_result[:value]
              else
                errors << artists_result[:error]
                []
              end

    movies = if movies_result[:success]
               movies_result[:value]
             else
               errors << movies_result[:error]
               []
             end

    subjects = artists.map(&:name) + movies.map(&:name)

    Ch08IO::IO.pure(
      TravelGuide.new(
        attraction: attraction,
        subjects: subjects,
        search_report: SearchReport.new(
          attractions_searched: total_attractions,
          errors: errors
        )
      )
    )
  end

  # ===========================================================================
  # 11.10 Extended TravelGuide with Hotels
  # ===========================================================================

  # Extended travel guide with hotels
  # ホテル付き拡張旅行ガイド
  ExtendedTravelGuide = Struct.new(
    :attraction, :artists, :movies, :hotels, :search_report,
    keyword_init: true
  )

  # Build extended guide
  # 拡張ガイドを構築
  def self.extended_travel_guide(data_access, attraction_name)
    data_access.find_attractions(
      attraction_name,
      AttractionOrdering::BY_LOCATION_POPULATION,
      1
    ).bind do |attractions|
      if attractions.empty?
        Ch08IO::IO.pure(nil)
      else
        attraction = attractions.first
        build_extended_guide(data_access, attraction)
      end
    end
  end

  def self.build_extended_guide(data_access, attraction)
    Ch08IO::IO.delay do
      location_id = attraction.location.id
      errors = []

      artists_result = data_access.find_artists_from_location(location_id, 3).run!
      movies_result = data_access.find_movies_about_location(location_id, 3).run!
      hotels_result = data_access.find_hotels_near_location(location_id, 5).run!

      artists = extract_result(artists_result, errors)
      movies = extract_result(movies_result, errors)
      hotels = extract_result(hotels_result, errors)

      ExtendedTravelGuide.new(
        attraction: attraction,
        artists: artists,
        movies: movies,
        hotels: hotels,
        search_report: SearchReport.new(
          attractions_searched: 1,
          errors: errors
        )
      )
    end
  end

  def self.extract_result(result, errors)
    if result[:success]
      result[:value]
    else
      errors << result[:error]
      []
    end
  end

  # ===========================================================================
  # 11.11 Sample Data
  # ===========================================================================

  # Create sample data for testing
  # テスト用のサンプルデータを作成
  def self.create_sample_data
    sydney_id = LocationId.new('Q60')
    paris_id = LocationId.new('Q90')
    tokyo_id = LocationId.new('Q1490')

    sydney = Location.new(id: sydney_id, name: 'Sydney', population: 5_312_000)
    paris = Location.new(id: paris_id, name: 'Paris', population: 2_161_000)
    tokyo = Location.new(id: tokyo_id, name: 'Tokyo', population: 13_960_000)

    attractions = [
      Attraction.new(
        name: 'Sydney Opera House',
        description: 'Iconic performing arts venue',
        location: sydney
      ),
      Attraction.new(
        name: 'Eiffel Tower',
        description: 'Iron lattice tower on the Champ de Mars',
        location: paris
      ),
      Attraction.new(
        name: 'Tokyo Tower',
        description: 'Communications and observation tower',
        location: tokyo
      ),
      Attraction.new(
        name: 'Sydney Harbour Bridge',
        description: 'Steel through arch bridge',
        location: sydney
      )
    ]

    artists = {
      sydney_id => [
        MusicArtist.new(name: 'AC/DC', genre: 'Rock'),
        MusicArtist.new(name: 'INXS', genre: 'Rock'),
        MusicArtist.new(name: 'Sia', genre: 'Pop')
      ],
      paris_id => [
        MusicArtist.new(name: 'Daft Punk', genre: 'Electronic'),
        MusicArtist.new(name: 'Edith Piaf', genre: 'Chanson')
      ],
      tokyo_id => [
        MusicArtist.new(name: 'X Japan', genre: 'Rock'),
        MusicArtist.new(name: 'Hikaru Utada', genre: 'Pop')
      ]
    }

    movies = {
      sydney_id => [
        Movie.new(name: 'Finding Nemo', year: 2003),
        Movie.new(name: 'The Matrix', year: 1999)
      ],
      paris_id => [
        Movie.new(name: 'Amélie', year: 2001),
        Movie.new(name: 'Midnight in Paris', year: 2011)
      ],
      tokyo_id => [
        Movie.new(name: 'Lost in Translation', year: 2003),
        Movie.new(name: 'Kill Bill', year: 2003)
      ]
    }

    hotels = {
      sydney_id => [
        Hotel.new(name: 'Park Hyatt Sydney', rating: 4.8, location: sydney),
        Hotel.new(name: 'Four Seasons Sydney', rating: 4.7, location: sydney)
      ],
      paris_id => [
        Hotel.new(name: 'Ritz Paris', rating: 4.9, location: paris),
        Hotel.new(name: 'Le Meurice', rating: 4.8, location: paris)
      ],
      tokyo_id => [
        Hotel.new(name: 'Park Hyatt Tokyo', rating: 4.7, location: tokyo),
        Hotel.new(name: 'Aman Tokyo', rating: 4.9, location: tokyo)
      ]
    }

    {
      attractions: attractions,
      artists: artists,
      movies: movies,
      hotels: hotels
    }
  end

  # Create sample data access
  # サンプルデータアクセスを作成
  def self.create_sample_data_access
    data = create_sample_data
    InMemoryDataAccess.new(**data)
  end
end
