# frozen_string_literal: true

require 'spec_helper'
require 'ch08_io'

RSpec.describe Ch08IO do
  # ===========================================================================
  # 8.1 Basic IO Operations
  # ===========================================================================

  describe Ch08IO::IO do
    describe '.delay' do
      it 'defers execution until run!' do
        executed = false
        io = Ch08IO::IO.delay { executed = true; 42 }

        expect(executed).to be false
        result = io.run!
        expect(executed).to be true
        expect(result).to eq(42)
      end

      it 'executes each time run! is called' do
        counter = 0
        io = Ch08IO::IO.delay { counter += 1 }

        io.run!
        io.run!
        io.run!

        expect(counter).to eq(3)
      end
    end

    describe '.pure' do
      it 'wraps a value without side effects' do
        io = Ch08IO::IO.pure(42)
        expect(io.run!).to eq(42)
      end
    end

    describe '.unit' do
      it 'returns nil' do
        io = Ch08IO::IO.unit
        expect(io.run!).to be_nil
      end
    end

    describe '#fmap' do
      it 'transforms the result' do
        io = Ch08IO::IO.pure(5).fmap { |x| x * 2 }
        expect(io.run!).to eq(10)
      end

      it 'chains multiple transformations' do
        io = Ch08IO::IO.pure(2)
          .fmap { |x| x + 3 }
          .fmap { |x| x * 2 }
        expect(io.run!).to eq(10)
      end
    end

    describe '#bind' do
      it 'chains IO operations' do
        io = Ch08IO::IO.pure(5).bind do |x|
          Ch08IO::IO.pure(x * 2)
        end
        expect(io.run!).to eq(10)
      end

      it 'flattens nested IOs' do
        io = Ch08IO::IO.pure(3).bind do |a|
          Ch08IO::IO.pure(4).bind do |b|
            Ch08IO::IO.pure(a + b)
          end
        end
        expect(io.run!).to eq(7)
      end
    end

    describe '#or_else' do
      it 'returns original value on success' do
        io = Ch08IO::IO.pure(42).or_else(Ch08IO::IO.pure(0))
        expect(io.run!).to eq(42)
      end

      it 'returns fallback on failure' do
        io = Ch08IO::IO.delay { raise 'error' }.or_else(Ch08IO::IO.pure(0))
        expect(io.run!).to eq(0)
      end

      it 'chains multiple fallbacks' do
        io = Ch08IO::IO.delay { raise 'error1' }
          .or_else(Ch08IO::IO.delay { raise 'error2' })
          .or_else(Ch08IO::IO.pure(42))
        expect(io.run!).to eq(42)
      end
    end

    describe '#recover' do
      it 'handles errors with a recovery function' do
        io = Ch08IO::IO.delay { raise StandardError, 'test error' }
          .recover { |e| "Recovered from: #{e.message}" }
        expect(io.run!).to eq('Recovered from: test error')
      end

      it 'passes through on success' do
        io = Ch08IO::IO.pure(42).recover { |_e| 0 }
        expect(io.run!).to eq(42)
      end
    end

    describe '#product' do
      it 'combines two IOs into a pair' do
        io = Ch08IO::IO.pure(1).product(Ch08IO::IO.pure(2))
        expect(io.run!).to eq([1, 2])
      end
    end

    describe '#inspect' do
      it 'returns IO(...)' do
        io = Ch08IO::IO.pure(42)
        expect(io.inspect).to eq('IO(...)')
      end
    end
  end

  # ===========================================================================
  # 8.2 Casting Die Example
  # ===========================================================================

  describe '.cast_the_die' do
    it 'returns an IO' do
      io = described_class.cast_the_die
      expect(io).to be_a(Ch08IO::IO)
    end

    it 'returns a value between 1 and 6' do
      results = 100.times.map { described_class.cast_the_die.run! }
      expect(results.all? { |r| r >= 1 && r <= 6 }).to be true
    end
  end

  describe '.cast_the_die_twice' do
    it 'returns sum of two dice rolls' do
      results = 100.times.map { described_class.cast_the_die_twice.run! }
      expect(results.all? { |r| r >= 2 && r <= 12 }).to be true
    end
  end

  # ===========================================================================
  # 8.3 IO Creation
  # ===========================================================================

  describe '.delayed_print' do
    it 'returns an IO' do
      io = described_class.delayed_print('test')
      expect(io).to be_a(Ch08IO::IO)
    end
  end

  describe '.pure_value' do
    it 'wraps a value in IO' do
      io = described_class.pure_value(42)
      expect(io.run!).to eq(42)
    end
  end

  # ===========================================================================
  # 8.4 Meeting Scheduling Example
  # ===========================================================================

  describe Ch08IO::MeetingTime do
    it 'creates a meeting time struct' do
      meeting = Ch08IO::MeetingTime.new(start_hour: 9, end_hour: 10)
      expect(meeting.start_hour).to eq(9)
      expect(meeting.end_hour).to eq(10)
    end
  end

  describe '.calendar_entries' do
    it 'returns IO of meeting list' do
      io = described_class.calendar_entries('Alice')
      result = io.run!
      expect(result).to be_an(Array)
      expect(result.all? { |m| m.is_a?(Ch08IO::MeetingTime) }).to be true
    end

    it 'returns empty list for unknown person' do
      io = described_class.calendar_entries('Unknown')
      expect(io.run!).to eq([])
    end
  end

  describe '.scheduled_meetings' do
    it 'combines meetings from two people' do
      io = described_class.scheduled_meetings('Alice', 'Bob')
      result = io.run!
      expect(result.size).to be >= 2
    end
  end

  describe '.meetings_overlap?' do
    it 'detects overlapping meetings' do
      m1 = Ch08IO::MeetingTime.new(start_hour: 9, end_hour: 11)
      m2 = Ch08IO::MeetingTime.new(start_hour: 10, end_hour: 12)
      expect(described_class.meetings_overlap?(m1, m2)).to be true
    end

    it 'detects non-overlapping meetings' do
      m1 = Ch08IO::MeetingTime.new(start_hour: 9, end_hour: 10)
      m2 = Ch08IO::MeetingTime.new(start_hour: 10, end_hour: 11)
      expect(described_class.meetings_overlap?(m1, m2)).to be false
    end

    it 'detects contained meetings' do
      m1 = Ch08IO::MeetingTime.new(start_hour: 9, end_hour: 12)
      m2 = Ch08IO::MeetingTime.new(start_hour: 10, end_hour: 11)
      expect(described_class.meetings_overlap?(m1, m2)).to be true
    end
  end

  describe '.possible_meetings' do
    it 'finds available slots' do
      existing = [
        Ch08IO::MeetingTime.new(start_hour: 9, end_hour: 10),
        Ch08IO::MeetingTime.new(start_hour: 14, end_hour: 15)
      ]
      result = described_class.possible_meetings(existing, 8, 18, 1)
      expect(result).not_to be_empty
      expect(result.none? { |s| described_class.meetings_overlap?(s, existing[0]) }).to be true
    end

    it 'returns empty when no slots available' do
      existing = (8..17).map do |hour|
        Ch08IO::MeetingTime.new(start_hour: hour, end_hour: hour + 1)
      end
      result = described_class.possible_meetings(existing, 8, 18, 1)
      expect(result).to be_empty
    end
  end

  # ===========================================================================
  # 8.5 Retry Strategies
  # ===========================================================================

  describe '.retry_action' do
    it 'succeeds on first try' do
      io = described_class.retry_action(Ch08IO::IO.pure(42), 3)
      expect(io.run!).to eq(42)
    end

    it 'retries on failure' do
      attempts = 0
      action = Ch08IO::IO.delay do
        attempts += 1
        raise 'error' if attempts < 3

        'success'
      end
      io = described_class.retry_action(action, 5)
      expect(io.run!).to eq('success')
      expect(attempts).to eq(3)
    end
  end

  describe '.retry_with_default' do
    it 'returns value on success' do
      io = described_class.retry_with_default(Ch08IO::IO.pure(42), 3, 0)
      expect(io.run!).to eq(42)
    end

    it 'returns default after all retries fail' do
      io = described_class.retry_with_default(
        Ch08IO::IO.delay { raise 'error' },
        3,
        0
      )
      expect(io.run!).to eq(0)
    end
  end

  # ===========================================================================
  # 8.6 Sequence
  # ===========================================================================

  describe '.sequence' do
    it 'converts array of IOs to IO of array' do
      ios = [Ch08IO::IO.pure(1), Ch08IO::IO.pure(2), Ch08IO::IO.pure(3)]
      io = described_class.sequence(ios)
      expect(io.run!).to eq([1, 2, 3])
    end

    it 'handles empty array' do
      io = described_class.sequence([])
      expect(io.run!).to eq([])
    end
  end

  describe '.traverse' do
    it 'maps and sequences' do
      io = described_class.traverse([1, 2, 3]) { |x| Ch08IO::IO.pure(x * 2) }
      expect(io.run!).to eq([2, 4, 6])
    end
  end

  describe '.scheduled_meetings_for_all' do
    it 'gets meetings for all attendees' do
      io = described_class.scheduled_meetings_for_all(%w[Alice Bob])
      result = io.run!
      expect(result).to be_an(Array)
    end
  end

  # ===========================================================================
  # 8.7 Combining IOs
  # ===========================================================================

  describe '.combine_io' do
    it 'combines two IOs with a function' do
      io = described_class.combine_io(
        Ch08IO::IO.pure(3),
        Ch08IO::IO.pure(4)
      ) { |a, b| a + b }
      expect(io.run!).to eq(7)
    end
  end

  describe '.map2' do
    it 'maps over two IOs' do
      io = described_class.map2(
        Ch08IO::IO.pure('hello'),
        Ch08IO::IO.pure('world')
      ) { |a, b| "#{a} #{b}" }
      expect(io.run!).to eq('hello world')
    end
  end

  # ===========================================================================
  # 8.8 Practical Examples
  # ===========================================================================

  describe '.print_and_return' do
    it 'returns the message' do
      io = described_class.print_and_return('test')
      expect(io.run!).to eq('test')
    end
  end

  describe '.current_time' do
    it 'returns current time' do
      before = Time.now
      time = described_class.current_time.run!
      after = Time.now
      expect(time).to be >= before
      expect(time).to be <= after
    end
  end

  # ===========================================================================
  # 8.9 Meeting Workflow
  # ===========================================================================

  describe '.schedule_meeting' do
    it 'finds possible meeting times' do
      io = described_class.schedule_meeting('Alice', 'Bob', 1)
      result = io.run!
      expect(result).to be_an(Array)
    end
  end

  describe '.meeting_workflow' do
    it 'returns first available meeting time' do
      io = described_class.meeting_workflow('Alice', 'Charlie', 1)
      result = io.run!
      if result
        expect(result).to be_a(Ch08IO::MeetingTime)
      end
    end
  end
end
