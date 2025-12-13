# frozen_string_literal: true

require 'spec_helper'
require 'ch10_concurrency'

RSpec.describe Ch10Concurrency do
  # ===========================================================================
  # 10.1 Ref - Atomic Shared State
  # ===========================================================================

  describe Ch10Concurrency::Ref do
    describe '.of' do
      it 'creates a Ref with initial value' do
        io = Ch10Concurrency::Ref.of(42)
        ref = io.run!
        expect(ref.get.run!).to eq(42)
      end
    end

    describe '#get' do
      it 'returns current value' do
        ref = Ch10Concurrency::Ref.new(10)
        expect(ref.get.run!).to eq(10)
      end
    end

    describe '#set' do
      it 'sets new value' do
        ref = Ch10Concurrency::Ref.new(0)
        ref.set(100).run!
        expect(ref.get.run!).to eq(100)
      end
    end

    describe '#update' do
      it 'updates value atomically' do
        ref = Ch10Concurrency::Ref.new(5)
        ref.update { |x| x * 2 }.run!
        expect(ref.get.run!).to eq(10)
      end

      it 'chains multiple updates' do
        ref = Ch10Concurrency::Ref.new(0)
        ref.update { |x| x + 1 }.run!
        ref.update { |x| x + 1 }.run!
        ref.update { |x| x + 1 }.run!
        expect(ref.get.run!).to eq(3)
      end
    end

    describe '#modify' do
      it 'modifies and returns result' do
        ref = Ch10Concurrency::Ref.new(10)
        result = ref.modify { |x| [x + 5, x] }.run!
        expect(result).to eq(10) # old value
        expect(ref.get.run!).to eq(15) # new value
      end
    end

    describe '#update_and_get' do
      it 'updates and returns new value' do
        ref = Ch10Concurrency::Ref.new(5)
        result = ref.update_and_get { |x| x * 3 }.run!
        expect(result).to eq(15)
      end
    end

    describe '#get_and_update' do
      it 'returns old value and updates' do
        ref = Ch10Concurrency::Ref.new(5)
        result = ref.get_and_update { |x| x * 3 }.run!
        expect(result).to eq(5) # old value
        expect(ref.get.run!).to eq(15) # new value
      end
    end

    describe 'thread safety' do
      it 'handles concurrent updates correctly' do
        ref = Ch10Concurrency::Ref.new(0)

        threads = 100.times.map do
          Thread.new do
            10.times { ref.update { |x| x + 1 }.run! }
          end
        end

        threads.each(&:join)
        expect(ref.get.run!).to eq(1000)
      end
    end
  end

  # ===========================================================================
  # 10.2 Parallel Execution
  # ===========================================================================

  describe '.par_sequence' do
    it 'executes IOs in parallel' do
      ios = [
        Ch08IO::IO.pure(1),
        Ch08IO::IO.pure(2),
        Ch08IO::IO.pure(3)
      ]
      result = described_class.par_sequence(ios).run!
      expect(result).to eq([1, 2, 3])
    end

    it 'actually runs in parallel' do
      start_time = Time.now
      ios = 3.times.map do
        Ch08IO::IO.delay { sleep(0.1); 1 }
      end
      result = described_class.par_sequence(ios).run!
      elapsed = Time.now - start_time

      expect(result).to eq([1, 1, 1])
      expect(elapsed).to be < 0.25 # Should be ~0.1s, not 0.3s
    end
  end

  describe '.par_traverse' do
    it 'maps and executes in parallel' do
      result = described_class.par_traverse([1, 2, 3]) do |x|
        Ch08IO::IO.pure(x * 2)
      end.run!
      expect(result).to eq([2, 4, 6])
    end
  end

  describe '.par_map2' do
    it 'combines two IOs in parallel' do
      result = described_class.par_map2(
        Ch08IO::IO.pure(3),
        Ch08IO::IO.pure(4)
      ) { |a, b| a + b }.run!
      expect(result).to eq(7)
    end
  end

  describe '.par_both' do
    it 'returns both results' do
      result = described_class.par_both(
        Ch08IO::IO.pure('hello'),
        Ch08IO::IO.pure('world')
      ).run!
      expect(result).to eq(%w[hello world])
    end
  end

  describe '.race' do
    it 'returns the first to complete' do
      io1 = Ch08IO::IO.delay { sleep(0.2); 'slow' }
      io2 = Ch08IO::IO.delay { sleep(0.05); 'fast' }

      result = described_class.race(io1, io2).run!
      expect(result).to eq('fast')
    end
  end

  # ===========================================================================
  # 10.3 Fiber
  # ===========================================================================

  describe 'FiberHandle' do
    describe 'Ch10Concurrency.start' do
      it 'starts an IO in the background' do
        counter = Ch10Concurrency::Ref.new(0)
        io = counter.update { |x| x + 1 }

        fiber = Ch10Concurrency.start(io).run!
        fiber.join.run!

        expect(counter.get.run!).to eq(1)
      end
    end

    describe '#cancel' do
      it 'cancels a running fiber' do
        counter = Ch10Concurrency::Ref.new(0)
        io = Ch08IO::IO.delay do
          loop do
            counter.update { |x| x + 1 }.run!
            sleep(0.01)
          end
        end

        fiber = Ch10Concurrency.start(io).run!
        sleep(0.05)
        fiber.cancel.run!

        count_at_cancel = counter.get.run!
        sleep(0.05)
        count_after = counter.get.run!

        # Should stop incrementing after cancel
        expect(count_after).to be <= count_at_cancel + 1
      end
    end
  end

  # ===========================================================================
  # 10.4 Forever and Repeat
  # ===========================================================================

  describe '.repeat_n' do
    it 'runs IO n times' do
      counter = 0
      io = Ch08IO::IO.delay { counter += 1 }
      described_class.repeat_n(io, 5).run!
      expect(counter).to eq(5)
    end
  end

  # ===========================================================================
  # 10.5 Sleep
  # ===========================================================================

  describe '.sleep_io' do
    it 'sleeps for specified seconds' do
      start = Time.now
      described_class.sleep_io(0.1).run!
      elapsed = Time.now - start
      expect(elapsed).to be >= 0.09
    end
  end

  describe '.sleep_millis' do
    it 'sleeps for specified milliseconds' do
      start = Time.now
      described_class.sleep_millis(100).run!
      elapsed = Time.now - start
      expect(elapsed).to be >= 0.09
    end
  end

  # ===========================================================================
  # 10.6 Check-ins Example
  # ===========================================================================

  describe Ch10Concurrency::City do
    it 'creates a city struct' do
      city = Ch10Concurrency::City.new(name: 'Sydney')
      expect(city.name).to eq('Sydney')
    end
  end

  describe Ch10Concurrency::CityStats do
    it 'creates a city stats struct' do
      city = Ch10Concurrency::City.new(name: 'Sydney')
      stats = Ch10Concurrency::CityStats.new(city: city, check_ins: 100)
      expect(stats.city.name).to eq('Sydney')
      expect(stats.check_ins).to eq(100)
    end
  end

  describe '.top_cities' do
    it 'returns top N cities by check-ins' do
      cities = {
        Ch10Concurrency::City.new(name: 'Sydney') => 100,
        Ch10Concurrency::City.new(name: 'Dublin') => 200,
        Ch10Concurrency::City.new(name: 'Lima') => 50
      }

      result = described_class.top_cities(cities, 2)

      expect(result.size).to eq(2)
      expect(result[0].city.name).to eq('Dublin')
      expect(result[0].check_ins).to eq(200)
      expect(result[1].city.name).to eq('Sydney')
    end
  end

  describe '.store_check_in' do
    it 'increments city check-in count' do
      ref = Ch10Concurrency::Ref.new({})
      city = Ch10Concurrency::City.new(name: 'Sydney')

      described_class.store_check_in(ref, city).run!
      described_class.store_check_in(ref, city).run!
      described_class.store_check_in(ref, city).run!

      check_ins = ref.get.run!
      expect(check_ins[city]).to eq(3)
    end
  end

  describe '.process_check_ins_concurrent' do
    it 'processes check-ins and returns ranking' do
      cities = [
        Ch10Concurrency::City.new(name: 'Sydney'),
        Ch10Concurrency::City.new(name: 'Dublin'),
        Ch10Concurrency::City.new(name: 'Sydney'),
        Ch10Concurrency::City.new(name: 'Lima'),
        Ch10Concurrency::City.new(name: 'Sydney')
      ]

      result = described_class.process_check_ins_concurrent(cities).run!

      expect(result).to be_an(Array)
      expect(result.first.city.name).to eq('Sydney')
      expect(result.first.check_ins).to eq(3)
    end
  end

  # ===========================================================================
  # 10.8 Parallel Die Casting
  # ===========================================================================

  describe '.cast_die_twice_parallel' do
    it 'returns sum of two dice' do
      results = 50.times.map { described_class.cast_die_twice_parallel.run! }
      expect(results.all? { |r| r >= 2 && r <= 12 }).to be true
    end
  end

  describe '.cast_die_n_parallel' do
    it 'returns n dice results' do
      result = described_class.cast_die_n_parallel(5).run!
      expect(result.size).to eq(5)
      expect(result.all? { |r| r >= 1 && r <= 6 }).to be true
    end
  end

  describe '.cast_die_n_with_ref' do
    it 'stores results in Ref' do
      result = described_class.cast_die_n_with_ref(5).run!
      expect(result.size).to eq(5)
      expect(result.all? { |r| r >= 1 && r <= 6 }).to be true
    end
  end

  # ===========================================================================
  # 10.9 Timeout Operations
  # ===========================================================================

  describe '.with_timeout' do
    it 'returns success when completed in time' do
      io = Ch08IO::IO.delay { sleep(0.05); 42 }
      result = described_class.with_timeout(io, 0.2).run!

      expect(result[:success]).to be true
      expect(result[:value]).to eq(42)
    end

    it 'returns failure on timeout' do
      io = Ch08IO::IO.delay { sleep(0.5); 42 }
      result = described_class.with_timeout(io, 0.1).run!

      expect(result[:success]).to be false
      expect(result[:error]).to eq('Timeout')
    end
  end

  describe '.collect_for' do
    it 'collects results for duration' do
      counter = 0
      producer = Ch08IO::IO.delay do
        counter += 1
        sleep(0.02)
        counter
      end

      result = described_class.collect_for(producer, 0.1).run!

      expect(result).to be_an(Array)
      expect(result.size).to be >= 2
    end
  end

  # ===========================================================================
  # 10.10 Parallel Map Updates
  # ===========================================================================

  describe '.apply_updates' do
    it 'applies updates to map' do
      updates = [
        Ch10Concurrency::Update.new(key: 'a', value: 1),
        Ch10Concurrency::Update.new(key: 'b', value: 2),
        Ch10Concurrency::Update.new(key: 'c', value: 3)
      ]

      result = described_class.apply_updates(updates).run!

      expect(result).to eq({ 'a' => 1, 'b' => 2, 'c' => 3 })
    end

    it 'handles duplicate keys' do
      updates = [
        Ch10Concurrency::Update.new(key: 'a', value: 1),
        Ch10Concurrency::Update.new(key: 'a', value: 2)
      ]

      result = described_class.apply_updates(updates).run!

      # Last update wins (though order may vary due to parallelism)
      expect(result['a']).to be_between(1, 2)
    end
  end

  # ===========================================================================
  # 10.11 Counter Examples
  # ===========================================================================

  describe '.increment_sequential' do
    it 'increments counter n times' do
      result = described_class.increment_sequential(10).run!
      expect(result).to eq(10)
    end
  end

  describe '.increment_parallel' do
    it 'increments counter n times in parallel' do
      result = described_class.increment_parallel(100).run!
      expect(result).to eq(100)
    end
  end

  describe '.count_evens' do
    it 'counts even numbers from IOs' do
      ios = [
        Ch08IO::IO.pure(2),
        Ch08IO::IO.pure(3),
        Ch08IO::IO.pure(4),
        Ch08IO::IO.pure(5),
        Ch08IO::IO.pure(6)
      ]

      result = described_class.count_evens(ios).run!
      expect(result).to eq(3) # 2, 4, 6
    end
  end

  # ===========================================================================
  # 10.12 Producer-Consumer Pattern
  # ===========================================================================

  describe Ch10Concurrency::RefQueue do
    let(:queue) { Ch10Concurrency::RefQueue.new }

    describe '#enqueue' do
      it 'adds items to queue' do
        queue.enqueue(1).run!
        queue.enqueue(2).run!
        expect(queue.to_array.run!).to eq([1, 2])
      end
    end

    describe '#dequeue' do
      it 'removes and returns first item' do
        queue.enqueue(1).run!
        queue.enqueue(2).run!

        result = queue.dequeue.run!
        expect(result).to eq(1)
        expect(queue.to_array.run!).to eq([2])
      end

      it 'returns nil for empty queue' do
        result = queue.dequeue.run!
        expect(result).to be_nil
      end
    end

    describe '#size' do
      it 'returns queue size' do
        queue.enqueue(1).run!
        queue.enqueue(2).run!
        expect(queue.size.run!).to eq(2)
      end
    end
  end

  describe '.producer_consumer' do
    it 'produces and consumes items' do
      counter = 0
      producer = Ch08IO::IO.delay do
        counter += 1
        sleep(0.01)
        counter
      end
      consumer_fn = ->(x) { x * 2 }

      result = described_class.producer_consumer(producer, consumer_fn, 0.1).run!

      expect(result).to be_an(Array)
      expect(result.size).to be >= 1
    end
  end
end
