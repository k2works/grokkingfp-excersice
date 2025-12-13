# frozen_string_literal: true

require 'spec_helper'
require 'ch11_application'

RSpec.describe Ch11Application do
  # ===========================================================================
  # 11.1 Domain Models
  # ===========================================================================

  describe Ch11Application::LocationId do
    it 'creates a location id' do
      id = Ch11Application::LocationId.new('Q123')
      expect(id.value).to eq('Q123')
    end

    it 'compares equal ids' do
      id1 = Ch11Application::LocationId.new('Q123')
      id2 = Ch11Application::LocationId.new('Q123')
      expect(id1).to eq(id2)
    end

    it 'compares different ids' do
      id1 = Ch11Application::LocationId.new('Q123')
      id2 = Ch11Application::LocationId.new('Q456')
      expect(id1).not_to eq(id2)
    end

    it 'works as hash key' do
      id1 = Ch11Application::LocationId.new('Q123')
      id2 = Ch11Application::LocationId.new('Q123')
      hash = { id1 => 'value' }
      expect(hash[id2]).to eq('value')
    end
  end

  describe Ch11Application::Location do
    it 'creates a location' do
      id = Ch11Application::LocationId.new('Q60')
      location = Ch11Application::Location.new(id: id, name: 'Sydney', population: 5_312_000)

      expect(location.name).to eq('Sydney')
      expect(location.population).to eq(5_312_000)
    end
  end

  describe Ch11Application::Attraction do
    it 'creates an attraction' do
      id = Ch11Application::LocationId.new('Q60')
      location = Ch11Application::Location.new(id: id, name: 'Sydney', population: 5_312_000)
      attraction = Ch11Application::Attraction.new(
        name: 'Sydney Opera House',
        description: 'Iconic venue',
        location: location
      )

      expect(attraction.name).to eq('Sydney Opera House')
      expect(attraction.location.name).to eq('Sydney')
    end
  end

  describe Ch11Application::SearchReport do
    describe '.empty' do
      it 'creates an empty report' do
        report = Ch11Application::SearchReport.empty
        expect(report.attractions_searched).to eq(0)
        expect(report.errors).to eq([])
      end
    end

    describe '#add_error' do
      it 'adds an error to the report' do
        report = Ch11Application::SearchReport.empty
        updated = report.add_error('Network error')
        expect(updated.errors).to eq(['Network error'])
      end

      it 'preserves existing errors' do
        report = Ch11Application::SearchReport.new(attractions_searched: 1, errors: ['Error 1'])
        updated = report.add_error('Error 2')
        expect(updated.errors).to eq(['Error 1', 'Error 2'])
      end
    end

    describe '#with_attractions_count' do
      it 'updates the attractions count' do
        report = Ch11Application::SearchReport.empty
        updated = report.with_attractions_count(5)
        expect(updated.attractions_searched).to eq(5)
      end
    end
  end

  # ===========================================================================
  # 11.2 InMemoryDataAccess
  # ===========================================================================

  describe Ch11Application::InMemoryDataAccess do
    let(:sample_data) { Ch11Application.create_sample_data }
    let(:data_access) { Ch11Application::InMemoryDataAccess.new(**sample_data) }

    describe '#find_attractions' do
      it 'finds attractions by name' do
        result = data_access.find_attractions('Sydney', :by_name, 10).run!
        expect(result.size).to eq(2)
        expect(result.map(&:name)).to include('Sydney Opera House')
      end

      it 'respects limit' do
        result = data_access.find_attractions('', :by_name, 2).run!
        expect(result.size).to be <= 2
      end

      it 'sorts by name' do
        result = data_access.find_attractions(
          '',
          Ch11Application::AttractionOrdering::BY_NAME,
          10
        ).run!
        names = result.map(&:name)
        expect(names).to eq(names.sort)
      end

      it 'sorts by population descending' do
        result = data_access.find_attractions(
          '',
          Ch11Application::AttractionOrdering::BY_LOCATION_POPULATION,
          10
        ).run!
        populations = result.map { |a| a.location.population }
        expect(populations).to eq(populations.sort.reverse)
      end
    end

    describe '#find_artists_from_location' do
      it 'finds artists for a location' do
        sydney_id = Ch11Application::LocationId.new('Q60')
        result = data_access.find_artists_from_location(sydney_id, 10).run!

        expect(result[:success]).to be true
        expect(result[:value].size).to be >= 1
      end

      it 'returns empty for unknown location' do
        unknown_id = Ch11Application::LocationId.new('UNKNOWN')
        result = data_access.find_artists_from_location(unknown_id, 10).run!

        expect(result[:success]).to be true
        expect(result[:value]).to eq([])
      end
    end

    describe '#find_movies_about_location' do
      it 'finds movies for a location' do
        sydney_id = Ch11Application::LocationId.new('Q60')
        result = data_access.find_movies_about_location(sydney_id, 10).run!

        expect(result[:success]).to be true
        expect(result[:value].size).to be >= 1
      end
    end

    describe '#find_hotels_near_location' do
      it 'finds hotels for a location' do
        sydney_id = Ch11Application::LocationId.new('Q60')
        result = data_access.find_hotels_near_location(sydney_id, 10).run!

        expect(result[:success]).to be true
        expect(result[:value].size).to be >= 1
      end
    end
  end

  # ===========================================================================
  # 11.3 CachedDataAccess
  # ===========================================================================

  describe Ch11Application::CachedDataAccess do
    let(:base_access) { Ch11Application.create_sample_data_access }
    let(:cached_access) { Ch11Application::CachedDataAccess.new(base_access) }

    describe '#find_attractions' do
      it 'caches results' do
        # First call
        result1 = cached_access.find_attractions('Sydney', :by_name, 10).run!

        # Second call should return cached
        result2 = cached_access.find_attractions('Sydney', :by_name, 10).run!

        expect(result1).to eq(result2)
        expect(cached_access.cache_stats[:attractions]).to eq(1)
      end

      it 'caches different queries separately' do
        cached_access.find_attractions('Sydney', :by_name, 10).run!
        cached_access.find_attractions('Tokyo', :by_name, 10).run!

        expect(cached_access.cache_stats[:attractions]).to eq(2)
      end
    end

    describe '#cache_stats' do
      it 'returns cache statistics' do
        stats = cached_access.cache_stats
        expect(stats).to have_key(:attractions)
        expect(stats).to have_key(:artists)
        expect(stats).to have_key(:movies)
        expect(stats).to have_key(:hotels)
      end
    end
  end

  # ===========================================================================
  # 11.4 FailingDataAccess
  # ===========================================================================

  describe Ch11Application::FailingDataAccess do
    let(:base_access) { Ch11Application.create_sample_data_access }

    it 'passes through when no failures configured' do
      failing_access = Ch11Application::FailingDataAccess.new(base_access)
      result = failing_access.find_attractions('Sydney', :by_name, 10).run!
      expect(result.size).to be >= 1
    end

    it 'returns error for configured failures' do
      failing_access = Ch11Application::FailingDataAccess.new(
        base_access,
        failing_methods: [:find_artists_from_location]
      )
      sydney_id = Ch11Application::LocationId.new('Q60')
      result = failing_access.find_artists_from_location(sydney_id, 10).run!

      expect(result[:success]).to be false
      expect(result[:error]).to include('Network error')
    end
  end

  # ===========================================================================
  # 11.5 Resource
  # ===========================================================================

  describe Ch11Application::Resource do
    describe '.make' do
      it 'acquires and releases resources' do
        acquired = false
        released = false

        resource = Ch11Application::Resource.make(
          Ch08IO::IO.delay { acquired = true; 'resource' }
        ) { |_r| released = true }

        result = resource.use { |r| r.upcase }.run!

        expect(acquired).to be true
        expect(released).to be true
        expect(result).to eq('RESOURCE')
      end

      it 'releases on error' do
        released = false

        resource = Ch11Application::Resource.make(
          Ch08IO::IO.pure('resource')
        ) { |_r| released = true }

        expect do
          resource.use { |_r| raise 'test error' }.run!
        end.to raise_error('test error')

        expect(released).to be true
      end
    end

    describe '#fmap' do
      it 'maps over the resource' do
        resource = Ch11Application::Resource.make(
          Ch08IO::IO.pure(5)
        ) { |_r| nil }

        result = resource.fmap { |n| n * 2 }.use { |n| n + 1 }.run!
        expect(result).to eq(11)
      end
    end
  end

  # ===========================================================================
  # 11.6 TravelGuide Application Logic
  # ===========================================================================

  describe '.travel_guide' do
    let(:data_access) { Ch11Application.create_sample_data_access }

    it 'returns a travel guide for valid attraction' do
      guide = Ch11Application.travel_guide(data_access, 'Sydney').run!

      expect(guide).not_to be_nil
      expect(guide.attraction.name).to include('Sydney')
      expect(guide.subjects).not_to be_empty
    end

    it 'returns nil for unknown attraction' do
      guide = Ch11Application.travel_guide(data_access, 'Unknown City XYZ').run!
      expect(guide).to be_nil
    end

    it 'includes search report' do
      guide = Ch11Application.travel_guide(data_access, 'Sydney').run!

      expect(guide.search_report).not_to be_nil
      expect(guide.search_report.attractions_searched).to be >= 1
    end

    it 'collects errors in search report' do
      failing_access = Ch11Application::FailingDataAccess.new(
        data_access,
        failing_methods: %i[find_artists_from_location find_movies_about_location]
      )

      guide = Ch11Application.travel_guide(failing_access, 'Sydney').run!

      expect(guide.search_report.errors.size).to eq(2)
    end
  end

  # ===========================================================================
  # 11.7 Extended TravelGuide
  # ===========================================================================

  describe '.extended_travel_guide' do
    let(:data_access) { Ch11Application.create_sample_data_access }

    it 'returns extended guide with hotels' do
      guide = Ch11Application.extended_travel_guide(data_access, 'Sydney').run!

      expect(guide).not_to be_nil
      expect(guide.artists).not_to be_empty
      expect(guide.movies).not_to be_empty
      expect(guide.hotels).not_to be_empty
    end

    it 'handles partial failures gracefully' do
      failing_access = Ch11Application::FailingDataAccess.new(
        data_access,
        failing_methods: [:find_hotels_near_location]
      )

      guide = Ch11Application.extended_travel_guide(failing_access, 'Sydney').run!

      expect(guide).not_to be_nil
      expect(guide.artists).not_to be_empty
      expect(guide.hotels).to be_empty
      expect(guide.search_report.errors.size).to eq(1)
    end
  end

  # ===========================================================================
  # 11.8 Sample Data
  # ===========================================================================

  describe '.create_sample_data' do
    it 'creates complete sample data' do
      data = Ch11Application.create_sample_data

      expect(data[:attractions]).not_to be_empty
      expect(data[:artists]).not_to be_empty
      expect(data[:movies]).not_to be_empty
      expect(data[:hotels]).not_to be_empty
    end
  end

  describe '.create_sample_data_access' do
    it 'creates working data access' do
      data_access = Ch11Application.create_sample_data_access
      result = data_access.find_attractions('', :by_name, 10).run!
      expect(result).not_to be_empty
    end
  end
end
