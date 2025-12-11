using Ch11;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch11.TravelGuideApp;

namespace GrokkingFP.CSharp.Tests.Ch11;

/// <summary>
/// 第11章: TravelGuide アプリケーションのテスト
/// </summary>
public class LocationIdTests
{
    [Fact]
    public void LocationId_HoldsValue()
    {
        var id = LocationId.Create("Q123");
        Assert.Equal("Q123", id.Value);
    }

    [Fact]
    public void LocationId_ToStringReturnsValue()
    {
        var id = LocationId.Create("Q123");
        Assert.Equal("Q123", id.ToString());
    }
}

public class LocationTests
{
    [Fact]
    public void Location_IsCreatedCorrectly()
    {
        var location = new Location(
            LocationId.Create("Q123"),
            "Tokyo",
            14000000
        );
        Assert.Equal("Tokyo", location.Name);
        Assert.Equal(14000000, location.Population);
    }
}

public class AttractionTests
{
    [Fact]
    public void Attraction_IsCreatedCorrectly()
    {
        var location = new Location(
            LocationId.Create("Q123"),
            "Tokyo",
            14000000
        );
        var attraction = new Attraction(
            "Tokyo Tower",
            Some("Famous landmark"),
            location
        );
        Assert.Equal("Tokyo Tower", attraction.Name);
        Assert.True(attraction.Description.IsSome);
        Assert.Equal("Famous landmark", attraction.Description.IfNone(""));
    }

    [Fact]
    public void Attraction_CanHaveNoDescription()
    {
        var location = new Location(
            LocationId.Create("Q123"),
            "Tokyo",
            14000000
        );
        var attraction = new Attraction("Tokyo Tower", None, location);
        Assert.True(attraction.Description.IsNone);
    }
}

public class TravelGuideModelTests
{
    [Fact]
    public void TravelGuide_IsCreatedCorrectly()
    {
        var location = new Location(
            LocationId.Create("Q123"),
            "Tokyo",
            14000000
        );
        var attraction = new Attraction("Tokyo Tower", None, location);
        var guide = new TravelGuide(attraction, Seq("Artist1", "Movie1"));

        Assert.Equal(2, guide.Subjects.Count);
        Assert.Contains("Artist1", guide.Subjects);
        Assert.Contains("Movie1", guide.Subjects);
    }
}

public class TestDataAccessTests
{
    [Fact]
    public async Task TestDataAccess_ReturnsCorrectData()
    {
        var testLocation = new Location(
            LocationId.Create("Q123"),
            "Test City",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Test Tower", Some("A test"), testLocation)
        );
        var testArtists = Seq(
            new MusicArtist("Test Artist", Some("Pop"))
        );
        var testMovies = Seq(
            new Movie("Test Movie", Some(1000000))
        );

        var dataAccess = TestDataAccess(testAttractions, testArtists, testMovies);

        var attractions = await dataAccess.FindAttractions("Test", AttractionOrdering.ByName, 10);
        Assert.Single(attractions);
        Assert.Equal("Test Tower", attractions.First().Name);
    }

    [Fact]
    public async Task TestDataAccess_Filters()
    {
        var testLocation = new Location(
            LocationId.Create("Q123"),
            "Test City",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Test Tower", None, testLocation),
            new Attraction("Other Place", None, testLocation)
        );

        var dataAccess = TestDataAccess(testAttractions, Seq<MusicArtist>(), Seq<Movie>());

        var attractions = await dataAccess.FindAttractions("Tower", AttractionOrdering.ByName, 10);
        Assert.Single(attractions);
        Assert.Equal("Test Tower", attractions.First().Name);
    }

    [Fact]
    public async Task TestDataAccess_RespectsLimit()
    {
        var testLocation = new Location(
            LocationId.Create("Q123"),
            "Test City",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Tower 1", None, testLocation),
            new Attraction("Tower 2", None, testLocation),
            new Attraction("Tower 3", None, testLocation)
        );

        var dataAccess = TestDataAccess(testAttractions, Seq<MusicArtist>(), Seq<Movie>());

        var attractions = await dataAccess.FindAttractions("Tower", AttractionOrdering.ByName, 2);
        Assert.Equal(2, attractions.Count);
    }
}

public class GetTravelGuideTests
{
    [Fact]
    public async Task GetTravelGuide_ReturnsGuideForValidAttraction()
    {
        var testLocation = new Location(
            LocationId.Create("Q123"),
            "Test City",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Test Tower", Some("A tower"), testLocation)
        );
        var testArtists = Seq(
            new MusicArtist("Artist1", None),
            new MusicArtist("Artist2", Some("Rock"))
        );
        var testMovies = Seq(
            new Movie("Movie1", None)
        );

        var dataAccess = TestDataAccess(testAttractions, testArtists, testMovies);
        var result = await GetTravelGuide(dataAccess, "Test");

        Assert.True(result.IsSome);
        var guide = result.IfNone(() => throw new Exception("Expected Some"));
        Assert.Equal("Test Tower", guide.Attraction.Name);
        Assert.Equal(3, guide.Subjects.Count); // 2 artists + 1 movie
    }

    [Fact]
    public async Task GetTravelGuide_ReturnsNoneWhenNotFound()
    {
        var dataAccess = TestDataAccess(Seq<Attraction>(), Seq<MusicArtist>(), Seq<Movie>());
        var result = await GetTravelGuide(dataAccess, "NonExistent");

        Assert.True(result.IsNone);
    }

    [Fact]
    public async Task GetTravelGuides_ReturnsMultipleGuides()
    {
        var testLocation1 = new Location(
            LocationId.Create("Q123"),
            "City1",
            200000
        );
        var testLocation2 = new Location(
            LocationId.Create("Q456"),
            "City2",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Tower1", None, testLocation1),
            new Attraction("Tower2", None, testLocation2)
        );
        var testArtists = Seq(new MusicArtist("Artist1", None));
        var testMovies = Seq(new Movie("Movie1", None));

        var dataAccess = TestDataAccess(testAttractions, testArtists, testMovies);
        var result = await GetTravelGuides(dataAccess, "Tower", 5);

        Assert.Equal(2, result.Count);
    }
}

public class CachedDataAccessTests
{
    [Fact]
    public async Task CachedDataAccess_CachesData()
    {
        var callCount = 0;
        var testLocation = new Location(
            LocationId.Create("Q123"),
            "Test City",
            100000
        );
        var testAttractions = Seq(
            new Attraction("Test Tower", None, testLocation)
        );

        var countingDataAccess = new CountingDataAccess(
            testAttractions,
            Seq<MusicArtist>(),
            Seq<Movie>(),
            () => callCount++
        );

        var cached = CachedDataAccess(countingDataAccess);

        // First call
        _ = await cached.FindAttractions("Test", AttractionOrdering.ByName, 10);
        Assert.Equal(1, callCount);

        // Second call (should be cached)
        _ = await cached.FindAttractions("Test", AttractionOrdering.ByName, 10);
        Assert.Equal(1, callCount); // Call count should not increase

        // Different parameters
        _ = await cached.FindAttractions("Other", AttractionOrdering.ByName, 10);
        Assert.Equal(2, callCount);
    }

    private class CountingDataAccess : IDataAccess
    {
        private readonly Seq<Attraction> _attractions;
        private readonly Seq<MusicArtist> _artists;
        private readonly Seq<Movie> _movies;
        private readonly Action _onCall;

        public CountingDataAccess(
            Seq<Attraction> attractions,
            Seq<MusicArtist> artists,
            Seq<Movie> movies,
            Action onCall)
        {
            _attractions = attractions;
            _artists = artists;
            _movies = movies;
            _onCall = onCall;
        }

        public Task<Seq<Attraction>> FindAttractions(string name, AttractionOrdering ordering, int limit)
        {
            _onCall();
            var filtered = _attractions.Filter(a => a.Name.Contains(name)).Take(limit);
            return Task.FromResult(filtered);
        }

        public Task<Seq<MusicArtist>> FindArtistsFromLocation(LocationId locationId, int limit)
        {
            return Task.FromResult(_artists.Take(limit));
        }

        public Task<Seq<Movie>> FindMoviesAboutLocation(LocationId locationId, int limit)
        {
            return Task.FromResult(_movies.Take(limit));
        }
    }
}

public class ResourceTests
{
    [Fact]
    public async Task Resource_Make_CreatesResource()
    {
        var acquired = false;
        var released = false;

        var resource = Resource.Make(
            async () =>
            {
                acquired = true;
                return "resource";
            },
            async r => { released = true; }
        );

        var result = await resource.UseAsync(async r => r + "-used");

        Assert.Equal("resource-used", result);
        Assert.True(acquired);
        Assert.True(released);
    }

    [Fact]
    public async Task Resource_ReleasesEvenOnException()
    {
        var released = false;

        var resource = Resource.Make(
            async () => "resource",
            async r => { released = true; }
        );

        try
        {
            await resource.UseAsync<string>(async r => throw new Exception("Test error"));
        }
        catch
        {
            // Expected
        }

        Assert.True(released);
    }
}

public class SampleDataTests
{
    [Fact]
    public void SampleLocation_HasCorrectValues()
    {
        Assert.Equal("Tokyo", SampleLocation.Name);
        Assert.Equal(14000000, SampleLocation.Population);
    }

    [Fact]
    public void SampleAttraction_HasCorrectValues()
    {
        Assert.Equal("Tokyo Tower", SampleAttraction.Name);
        Assert.True(SampleAttraction.Description.IsSome);
    }

    [Fact]
    public void SampleArtist_HasCorrectValues()
    {
        Assert.Equal("Hikaru Utada", SampleArtist.Name);
    }

    [Fact]
    public void SampleMovie_HasCorrectValues()
    {
        Assert.Equal("Lost in Translation", SampleMovie.Name);
    }
}
