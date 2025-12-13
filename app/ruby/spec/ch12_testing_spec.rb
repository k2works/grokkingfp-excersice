# frozen_string_literal: true

require 'spec_helper'
require 'ch12_testing'

RSpec.describe Ch12Testing do
  # ===========================================================================
  # 12.1 Test Helpers - TestDataAccess
  # ===========================================================================

  describe Ch12Testing::TestDataAccess do
    let(:location) { Ch12Testing::LocationBuilder.new.with_name('Sydney').build }
    let(:attraction) { Ch12Testing::AttractionBuilder.new.with_location(location).build }
    let(:artists) { [Ch11Application::MusicArtist.new(name: 'AC/DC', genre: 'Rock')] }
    let(:movies) { [Ch11Application::Movie.new(name: 'Finding Nemo', year: 2003)] }
    let(:hotels) { [Ch11Application::Hotel.new(name: 'Park Hyatt', rating: 4.8, location: location)] }

    let(:data_access) do
      Ch12Testing::TestDataAccess.new(
        attractions: [attraction],
        artists_by_location: { location.id => artists },
        movies_by_location: { location.id => movies },
        hotels_by_location: { location.id => hotels }
      )
    end

    describe '#find_attractions' do
      it 'finds attractions by name' do
        result = data_access.find_attractions('Test', :by_name, 10).run!
        expect(result.size).to eq(1)
      end

      it 'returns empty for non-matching name' do
        result = data_access.find_attractions('Unknown', :by_name, 10).run!
        expect(result).to be_empty
      end

      it 'respects limit' do
        attractions = 5.times.map { Ch12Testing::AttractionBuilder.new.build }
        access = Ch12Testing::TestDataAccess.new(attractions: attractions)
        result = access.find_attractions('', :by_name, 2).run!
        expect(result.size).to eq(2)
      end

      it 'sorts by name' do
        a1 = Ch12Testing::AttractionBuilder.new.with_name('Zoo').build
        a2 = Ch12Testing::AttractionBuilder.new.with_name('Aquarium').build
        a3 = Ch12Testing::AttractionBuilder.new.with_name('Museum').build
        access = Ch12Testing::TestDataAccess.new(attractions: [a1, a2, a3])

        result = access.find_attractions('', Ch11Application::AttractionOrdering::BY_NAME, 10).run!
        names = result.map(&:name)
        expect(names).to eq(%w[Aquarium Museum Zoo])
      end

      it 'sorts by population descending' do
        loc1 = Ch12Testing::LocationBuilder.new.with_population(100_000).build
        loc2 = Ch12Testing::LocationBuilder.new.with_population(500_000).build
        loc3 = Ch12Testing::LocationBuilder.new.with_population(200_000).build
        a1 = Ch12Testing::AttractionBuilder.new.with_location(loc1).build
        a2 = Ch12Testing::AttractionBuilder.new.with_location(loc2).build
        a3 = Ch12Testing::AttractionBuilder.new.with_location(loc3).build
        access = Ch12Testing::TestDataAccess.new(attractions: [a1, a2, a3])

        result = access.find_attractions(
          '',
          Ch11Application::AttractionOrdering::BY_LOCATION_POPULATION,
          10
        ).run!
        populations = result.map { |a| a.location.population }
        expect(populations).to eq([500_000, 200_000, 100_000])
      end
    end

    describe '#find_artists_from_location' do
      it 'returns artists for known location' do
        result = data_access.find_artists_from_location(location.id, 10).run!
        expect(result[:success]).to be true
        expect(result[:value]).to eq(artists)
      end

      it 'returns empty for unknown location' do
        unknown_id = Ch11Application::LocationId.new('UNKNOWN')
        result = data_access.find_artists_from_location(unknown_id, 10).run!
        expect(result[:success]).to be true
        expect(result[:value]).to eq([])
      end

      it 'returns error when configured' do
        access = Ch12Testing::TestDataAccess.new(
          artist_errors: { location.id => 'Network error' }
        )
        result = access.find_artists_from_location(location.id, 10).run!
        expect(result[:success]).to be false
        expect(result[:error]).to eq('Network error')
      end
    end

    describe '#find_movies_about_location' do
      it 'returns movies for known location' do
        result = data_access.find_movies_about_location(location.id, 10).run!
        expect(result[:success]).to be true
        expect(result[:value]).to eq(movies)
      end

      it 'returns error when configured' do
        access = Ch12Testing::TestDataAccess.new(
          movie_errors: { location.id => 'Timeout' }
        )
        result = access.find_movies_about_location(location.id, 10).run!
        expect(result[:success]).to be false
        expect(result[:error]).to eq('Timeout')
      end
    end

    describe '#find_hotels_near_location' do
      it 'returns hotels for known location' do
        result = data_access.find_hotels_near_location(location.id, 10).run!
        expect(result[:success]).to be true
        expect(result[:value]).to eq(hotels)
      end

      it 'returns error when configured' do
        access = Ch12Testing::TestDataAccess.new(
          hotel_errors: { location.id => 'Service unavailable' }
        )
        result = access.find_hotels_near_location(location.id, 10).run!
        expect(result[:success]).to be false
        expect(result[:error]).to eq('Service unavailable')
      end
    end
  end

  # ===========================================================================
  # 12.2 Test Data Builders
  # ===========================================================================

  describe Ch12Testing::LocationBuilder do
    it 'creates a location with defaults' do
      location = Ch12Testing::LocationBuilder.new.build
      expect(location.name).to eq('Test City')
      expect(location.population).to eq(100_000)
    end

    it 'allows customization via fluent interface' do
      location = Ch12Testing::LocationBuilder.new
                                             .with_name('Sydney')
                                             .with_population(5_000_000)
                                             .build
      expect(location.name).to eq('Sydney')
      expect(location.population).to eq(5_000_000)
    end

    it 'accepts string id' do
      location = Ch12Testing::LocationBuilder.new.with_id('Q123').build
      expect(location.id.value).to eq('Q123')
    end

    it 'accepts LocationId' do
      id = Ch11Application::LocationId.new('Q456')
      location = Ch12Testing::LocationBuilder.new.with_id(id).build
      expect(location.id).to eq(id)
    end
  end

  describe Ch12Testing::AttractionBuilder do
    it 'creates an attraction with defaults' do
      attraction = Ch12Testing::AttractionBuilder.new.build
      expect(attraction.name).to eq('Test Attraction')
      expect(attraction.description).to eq('A test attraction')
    end

    it 'allows customization' do
      attraction = Ch12Testing::AttractionBuilder.new
                                                 .with_name('Opera House')
                                                 .with_description('Iconic venue')
                                                 .build
      expect(attraction.name).to eq('Opera House')
      expect(attraction.description).to eq('Iconic venue')
    end

    it 'allows custom location' do
      location = Ch12Testing::LocationBuilder.new.with_name('Tokyo').build
      attraction = Ch12Testing::AttractionBuilder.new.with_location(location).build
      expect(attraction.location.name).to eq('Tokyo')
    end
  end

  # ===========================================================================
  # 12.3 Property-Based Testing Generators
  # ===========================================================================

  describe Ch12Testing::Gen do
    describe '.string' do
      it 'generates string of specified length' do
        str = Ch12Testing::Gen.string(15)
        expect(str.length).to eq(15)
      end

      it 'generates different strings' do
        strings = 10.times.map { Ch12Testing::Gen.string }
        expect(strings.uniq.size).to be > 1
      end
    end

    describe '.int' do
      it 'generates int in range' do
        100.times do
          n = Ch12Testing::Gen.int(10, 20)
          expect(n).to be_between(10, 20)
        end
      end
    end

    describe '.positive_int' do
      it 'generates positive integers' do
        100.times do
          n = Ch12Testing::Gen.positive_int
          expect(n).to be >= 1
        end
      end
    end

    describe '.boolean' do
      it 'generates both true and false' do
        results = 100.times.map { Ch12Testing::Gen.boolean }
        expect(results).to include(true)
        expect(results).to include(false)
      end
    end

    describe '.one_of' do
      it 'selects from options' do
        100.times do
          result = Ch12Testing::Gen.one_of('a', 'b', 'c')
          expect(%w[a b c]).to include(result)
        end
      end
    end

    describe '.list_of' do
      it 'generates list of specified size' do
        list = Ch12Testing::Gen.list_of(5) { Ch12Testing::Gen.int }
        expect(list.size).to eq(5)
      end
    end

    describe '.option' do
      it 'sometimes returns nil' do
        results = 100.times.map { Ch12Testing::Gen.option { 'value' } }
        expect(results).to include(nil)
        expect(results).to include('value')
      end
    end

    describe '.location_id' do
      it 'generates valid LocationId' do
        id = Ch12Testing::Gen.location_id
        expect(id).to be_a(Ch11Application::LocationId)
        expect(id.value).to match(/^Q\d+$/)
      end
    end

    describe '.location' do
      it 'generates valid Location' do
        location = Ch12Testing::Gen.location
        expect(location).to be_a(Ch11Application::Location)
        expect(location.name).not_to be_empty
        expect(location.population).to be > 0
      end
    end

    describe '.attraction' do
      it 'generates valid Attraction' do
        attraction = Ch12Testing::Gen.attraction
        expect(attraction).to be_a(Ch11Application::Attraction)
        expect(attraction.name).not_to be_empty
        expect(attraction.location).to be_a(Ch11Application::Location)
      end
    end

    describe '.music_artist' do
      it 'generates valid MusicArtist' do
        artist = Ch12Testing::Gen.music_artist
        expect(artist).to be_a(Ch11Application::MusicArtist)
        expect(artist.name).not_to be_empty
        expect(%w[Rock Pop Jazz Classical Electronic]).to include(artist.genre)
      end
    end

    describe '.movie' do
      it 'generates valid Movie' do
        movie = Ch12Testing::Gen.movie
        expect(movie).to be_a(Ch11Application::Movie)
        expect(movie.name).not_to be_empty
        expect(movie.year).to be_between(1900, 2024)
      end
    end

    describe '.hotel' do
      it 'generates valid Hotel' do
        hotel = Ch12Testing::Gen.hotel
        expect(hotel).to be_a(Ch11Application::Hotel)
        expect(hotel.name).not_to be_empty
        expect(hotel.rating).to be_between(3.0, 5.0)
      end
    end
  end

  # ===========================================================================
  # 12.4 Property Testing Framework
  # ===========================================================================

  describe Ch12Testing::PropertyTest do
    it 'runs iterations and collects failures' do
      test = Ch12Testing::PropertyTest.new('test', iterations: 10)
      test.for_all(-> { rand(10) }) { |n| n >= 0 }

      expect(test.passed?).to be true
      expect(test.failures).to be_empty
    end

    it 'records failures' do
      test = Ch12Testing::PropertyTest.new('test', iterations: 10)
      test.for_all(-> { rand(10) }) { |n| n > 100 } # Always fails

      expect(test.passed?).to be false
      expect(test.failures.size).to eq(10)
    end

    it 'handles exceptions' do
      test = Ch12Testing::PropertyTest.new('test', iterations: 5)
      test.for_all(-> { 'value' }) { |_| raise 'boom' }

      expect(test.passed?).to be false
      expect(test.failures.first[:error]).to eq('boom')
    end

    it 'provides summary' do
      test = Ch12Testing::PropertyTest.new('my test', iterations: 10)
      test.for_all(-> { 1 }) { |n| n == 1 }

      expect(test.summary).to eq('my test: PASSED (10 iterations)')
    end
  end

  # ===========================================================================
  # 12.5 Common Properties
  # ===========================================================================

  describe Ch12Testing::Properties do
    describe '.filter_bounded?' do
      it 'returns true when result is smaller' do
        expect(Ch12Testing::Properties.filter_bounded?([1, 2, 3], [1, 2])).to be true
      end

      it 'returns true when result is equal' do
        expect(Ch12Testing::Properties.filter_bounded?([1, 2], [1, 2])).to be true
      end

      it 'returns false when result is larger' do
        expect(Ch12Testing::Properties.filter_bounded?([1], [1, 2])).to be false
      end
    end

    describe '.filter_satisfies?' do
      it 'returns true when all satisfy predicate' do
        result = Ch12Testing::Properties.filter_satisfies?([2, 4, 6], &:even?)
        expect(result).to be true
      end

      it 'returns false when some do not satisfy' do
        result = Ch12Testing::Properties.filter_satisfies?([2, 3, 6], &:even?)
        expect(result).to be false
      end
    end

    describe '.limit_respected?' do
      it 'returns true when within limit' do
        expect(Ch12Testing::Properties.limit_respected?([1, 2], 5)).to be true
      end

      it 'returns false when exceeds limit' do
        expect(Ch12Testing::Properties.limit_respected?([1, 2, 3], 2)).to be false
      end
    end

    describe '.ordering_preserved?' do
      it 'returns true for sorted ascending' do
        result = Ch12Testing::Properties.ordering_preserved?([1, 2, 3]) { |a, b| a <= b }
        expect(result).to be true
      end

      it 'returns false for unsorted' do
        result = Ch12Testing::Properties.ordering_preserved?([1, 3, 2]) { |a, b| a <= b }
        expect(result).to be false
      end
    end

    describe '.errors_bounded?' do
      it 'returns true when errors within limit' do
        report = Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['e1', 'e2'])
        expect(Ch12Testing::Properties.errors_bounded?(report, 3)).to be true
      end

      it 'returns false when errors exceed limit' do
        report = Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['e1', 'e2', 'e3'])
        expect(Ch12Testing::Properties.errors_bounded?(report, 2)).to be false
      end
    end
  end

  # ===========================================================================
  # 12.6 Pure Function Testing
  # ===========================================================================

  describe '.filter_popular_locations' do
    it 'filters locations by population' do
      loc1 = Ch12Testing::LocationBuilder.new.with_population(100_000).build
      loc2 = Ch12Testing::LocationBuilder.new.with_population(500_000).build
      loc3 = Ch12Testing::LocationBuilder.new.with_population(200_000).build

      result = Ch12Testing.filter_popular_locations([loc1, loc2, loc3], 150_000)
      expect(result.size).to eq(2)
      expect(result.map(&:population)).to all(be >= 150_000)
    end

    it 'returns empty for no matches' do
      loc = Ch12Testing::LocationBuilder.new.with_population(100_000).build
      result = Ch12Testing.filter_popular_locations([loc], 500_000)
      expect(result).to be_empty
    end

    # Property-based test
    it 'always returns subset of input' do
      test = Ch12Testing::PropertyTest.new('filter returns subset', iterations: 50)
      test.for_all(
        -> { Ch12Testing::Gen.list_of(10) { Ch12Testing::Gen.location } },
        -> { Ch12Testing::Gen.positive_int(10_000_000) }
      ) do |locations, min_pop|
        result = Ch12Testing.filter_popular_locations(locations, min_pop)
        Ch12Testing::Properties.filter_bounded?(locations, result)
      end

      expect(test.passed?).to be true
    end
  end

  describe '.sort_attractions_by_name' do
    it 'sorts attractions alphabetically' do
      a1 = Ch12Testing::AttractionBuilder.new.with_name('Zoo').build
      a2 = Ch12Testing::AttractionBuilder.new.with_name('Aquarium').build
      a3 = Ch12Testing::AttractionBuilder.new.with_name('Museum').build

      result = Ch12Testing.sort_attractions_by_name([a1, a2, a3])
      expect(result.map(&:name)).to eq(%w[Aquarium Museum Zoo])
    end

    # Property-based test
    it 'preserves ordering property' do
      test = Ch12Testing::PropertyTest.new('sort preserves order', iterations: 50)
      test.for_all(
        -> { Ch12Testing::Gen.list_of(10) { Ch12Testing::Gen.attraction } }
      ) do |attractions|
        result = Ch12Testing.sort_attractions_by_name(attractions)
        Ch12Testing::Properties.ordering_preserved?(result) { |a, b| a.name <= b.name }
      end

      expect(test.passed?).to be true
    end
  end

  describe '.average_rating' do
    it 'calculates average' do
      loc = Ch12Testing::LocationBuilder.new.build
      hotels = [
        Ch11Application::Hotel.new(name: 'H1', rating: 4.0, location: loc),
        Ch11Application::Hotel.new(name: 'H2', rating: 5.0, location: loc),
        Ch11Application::Hotel.new(name: 'H3', rating: 3.0, location: loc)
      ]

      result = Ch12Testing.average_rating(hotels)
      expect(result).to eq(4.0)
    end

    it 'returns nil for empty list' do
      expect(Ch12Testing.average_rating([])).to be_nil
    end
  end

  describe '.group_by_genre' do
    it 'groups artists by genre' do
      artists = [
        Ch11Application::MusicArtist.new(name: 'A1', genre: 'Rock'),
        Ch11Application::MusicArtist.new(name: 'A2', genre: 'Pop'),
        Ch11Application::MusicArtist.new(name: 'A3', genre: 'Rock')
      ]

      result = Ch12Testing.group_by_genre(artists)
      expect(result['Rock'].size).to eq(2)
      expect(result['Pop'].size).to eq(1)
    end
  end

  # ===========================================================================
  # 12.7 Integration Test Helpers
  # ===========================================================================

  describe '.create_test_scenario' do
    it 'creates a complete test scenario' do
      data_access = Ch12Testing.create_test_scenario

      result = data_access.find_attractions('', :by_name, 10).run!
      expect(result).not_to be_empty
    end

    it 'uses provided options' do
      location = Ch12Testing::LocationBuilder.new.with_name('Custom City').build
      data_access = Ch12Testing.create_test_scenario(location: location)

      result = data_access.find_attractions('', :by_name, 10).run!
      expect(result.first.location.name).to eq('Custom City')
    end
  end

  describe '.create_failing_scenario' do
    it 'creates scenario with configured errors' do
      location = Ch12Testing::LocationBuilder.new.build
      attraction = Ch12Testing::AttractionBuilder.new.with_location(location).build
      data_access = Ch12Testing.create_failing_scenario(
        location: location,
        attraction: attraction,
        artist_error: 'Artist service down'
      )

      result = data_access.find_artists_from_location(location.id, 10).run!
      expect(result[:success]).to be false
      expect(result[:error]).to eq('Artist service down')
    end
  end

  # ===========================================================================
  # 12.8 Assertion Helpers
  # ===========================================================================

  describe Ch12Testing::Assertions do
    let(:valid_guide) do
      attraction = Ch12Testing::AttractionBuilder.new.build
      report = Ch11Application::SearchReport.empty
      Ch11Application::TravelGuide.new(
        attraction: attraction,
        subjects: ['Subject'],
        search_report: report
      )
    end

    describe '.assert_travel_guide_valid' do
      it 'returns true for valid guide' do
        expect(Ch12Testing::Assertions.assert_travel_guide_valid(valid_guide)).to be true
      end

      it 'raises for nil guide' do
        expect { Ch12Testing::Assertions.assert_travel_guide_valid(nil) }
          .to raise_error('Guide is nil')
      end
    end

    describe '.assert_no_errors' do
      it 'returns true when no errors' do
        expect(Ch12Testing::Assertions.assert_no_errors(valid_guide)).to be true
      end

      it 'raises when errors present' do
        guide_with_errors = Ch11Application::TravelGuide.new(
          attraction: valid_guide.attraction,
          subjects: ['Subject'],
          search_report: Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['Error'])
        )
        expect { Ch12Testing::Assertions.assert_no_errors(guide_with_errors) }
          .to raise_error(/Expected no errors/)
      end
    end

    describe '.assert_has_errors' do
      it 'returns true when errors present' do
        guide_with_errors = Ch11Application::TravelGuide.new(
          attraction: valid_guide.attraction,
          subjects: ['Subject'],
          search_report: Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['Error'])
        )
        expect(Ch12Testing::Assertions.assert_has_errors(guide_with_errors)).to be true
      end

      it 'raises when no errors' do
        expect { Ch12Testing::Assertions.assert_has_errors(valid_guide) }
          .to raise_error('Expected errors but got none')
      end

      it 'checks error count' do
        guide_with_errors = Ch11Application::TravelGuide.new(
          attraction: valid_guide.attraction,
          subjects: ['Subject'],
          search_report: Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['E1', 'E2'])
        )
        expect(Ch12Testing::Assertions.assert_has_errors(guide_with_errors, count: 2)).to be true
        expect { Ch12Testing::Assertions.assert_has_errors(guide_with_errors, count: 3) }
          .to raise_error(/Expected 3 errors but got 2/)
      end
    end

    describe '.assert_subjects_present' do
      it 'returns true when subjects present' do
        expect(Ch12Testing::Assertions.assert_subjects_present(valid_guide)).to be true
      end

      it 'raises when no subjects' do
        guide_without_subjects = Ch11Application::TravelGuide.new(
          attraction: valid_guide.attraction,
          subjects: [],
          search_report: Ch11Application::SearchReport.empty
        )
        expect { Ch12Testing::Assertions.assert_subjects_present(guide_without_subjects) }
          .to raise_error('Expected subjects but got none')
      end
    end
  end

  # ===========================================================================
  # 12.9 Test Fixtures
  # ===========================================================================

  describe Ch12Testing::Fixtures do
    describe '.sydney' do
      it 'returns Sydney location' do
        expect(Ch12Testing::Fixtures.sydney.name).to eq('Sydney')
        expect(Ch12Testing::Fixtures.sydney.population).to eq(5_312_000)
      end

      it 'returns same instance' do
        expect(Ch12Testing::Fixtures.sydney).to equal(Ch12Testing::Fixtures.sydney)
      end
    end

    describe '.sydney_opera_house' do
      it 'returns Sydney Opera House attraction' do
        expect(Ch12Testing::Fixtures.sydney_opera_house.name).to eq('Sydney Opera House')
        expect(Ch12Testing::Fixtures.sydney_opera_house.location.name).to eq('Sydney')
      end
    end

    describe '.sydney_artists' do
      it 'returns list of Sydney artists' do
        expect(Ch12Testing::Fixtures.sydney_artists.size).to eq(2)
        expect(Ch12Testing::Fixtures.sydney_artists.map(&:name)).to include('AC/DC')
      end
    end

    describe '.sydney_movies' do
      it 'returns list of Sydney movies' do
        expect(Ch12Testing::Fixtures.sydney_movies.size).to eq(2)
        expect(Ch12Testing::Fixtures.sydney_movies.map(&:name)).to include('Finding Nemo')
      end
    end

    describe '.sydney_hotels' do
      it 'returns list of Sydney hotels' do
        expect(Ch12Testing::Fixtures.sydney_hotels.size).to eq(1)
        expect(Ch12Testing::Fixtures.sydney_hotels.first.name).to eq('Park Hyatt Sydney')
      end
    end

    describe '.complete_data_access' do
      it 'returns working data access' do
        result = Ch12Testing::Fixtures.complete_data_access.find_attractions('Sydney', :by_name, 10).run!
        expect(result).not_to be_empty
      end
    end
  end

  # ===========================================================================
  # 12.10 Test Runner
  # ===========================================================================

  describe Ch12Testing::TestRunner do
    it 'runs all property tests' do
      runner = Ch12Testing::TestRunner.new

      runner.add_property('always true') do |test|
        test.for_all(-> { 1 }) { |n| n == 1 }
      end

      runner.add_property('also true') do |test|
        test.for_all(-> { 'a' }) { |s| s.is_a?(String) }
      end

      results = runner.run_all
      expect(results[:total]).to eq(2)
      expect(results[:passed]).to eq(2)
      expect(results[:failed]).to eq(0)
    end

    it 'reports failures' do
      runner = Ch12Testing::TestRunner.new

      runner.add_property('failing test') do |test|
        test.for_all(-> { 1 }) { |n| n > 100 }
      end

      results = runner.run_all
      expect(results[:failed]).to eq(1)
    end
  end

  # ===========================================================================
  # 12.11 Integration with TravelGuide
  # ===========================================================================

  describe 'TravelGuide integration tests' do
    it 'works with TestDataAccess' do
      location = Ch12Testing::Fixtures.sydney
      attraction = Ch12Testing::Fixtures.sydney_opera_house

      data_access = Ch12Testing::TestDataAccess.new(
        attractions: [attraction],
        artists_by_location: { location.id => Ch12Testing::Fixtures.sydney_artists },
        movies_by_location: { location.id => Ch12Testing::Fixtures.sydney_movies },
        hotels_by_location: { location.id => Ch12Testing::Fixtures.sydney_hotels }
      )

      guide = Ch11Application.travel_guide(data_access, 'Sydney').run!

      Ch12Testing::Assertions.assert_travel_guide_valid(guide)
      Ch12Testing::Assertions.assert_no_errors(guide)
      Ch12Testing::Assertions.assert_subjects_present(guide)
    end

    it 'handles errors gracefully' do
      location = Ch12Testing::Fixtures.sydney
      attraction = Ch12Testing::Fixtures.sydney_opera_house

      data_access = Ch12Testing::TestDataAccess.new(
        attractions: [attraction],
        artists_by_location: {},
        movies_by_location: {},
        hotels_by_location: {},
        artist_errors: { location.id => 'Network error' },
        movie_errors: { location.id => 'Timeout' }
      )

      guide = Ch11Application.travel_guide(data_access, 'Sydney').run!

      Ch12Testing::Assertions.assert_travel_guide_valid(guide)
      Ch12Testing::Assertions.assert_has_errors(guide, count: 2)
    end

    it 'extended guide works with fixtures' do
      guide = Ch11Application.extended_travel_guide(
        Ch12Testing::Fixtures.complete_data_access,
        'Sydney'
      ).run!

      expect(guide).not_to be_nil
      expect(guide.artists).not_to be_empty
      expect(guide.movies).not_to be_empty
      expect(guide.hotels).not_to be_empty
    end
  end
end
