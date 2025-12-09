package ch08;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch08.SchedulingMeetings.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 第8章: ミーティングスケジューリングのテスト
 */
@DisplayName("第8章: ミーティングスケジューリング")
class SchedulingMeetingsTest {

    @Nested
    @DisplayName("MeetingTime")
    class MeetingTimeTest {

        @Test
        @DisplayName("正常な MeetingTime を作成")
        void createValidMeetingTime() {
            MeetingTime meeting = new MeetingTime(9, 10);
            assertThat(meeting.startHour()).isEqualTo(9);
            assertThat(meeting.endHour()).isEqualTo(10);
            assertThat(meeting.lengthHours()).isEqualTo(1);
        }

        @Test
        @DisplayName("開始時間が終了時間以上はエラー")
        void startMustBeLessThanEnd() {
            assertThatThrownBy(() -> new MeetingTime(10, 10))
                    .isInstanceOf(IllegalArgumentException.class);
            assertThatThrownBy(() -> new MeetingTime(11, 10))
                    .isInstanceOf(IllegalArgumentException.class);
        }

        @Test
        @DisplayName("不正な時間はエラー")
        void invalidHours() {
            assertThatThrownBy(() -> new MeetingTime(-1, 10))
                    .isInstanceOf(IllegalArgumentException.class);
            assertThatThrownBy(() -> new MeetingTime(9, 25))
                    .isInstanceOf(IllegalArgumentException.class);
        }
    }

    @Nested
    @DisplayName("meetingsOverlap")
    class MeetingsOverlapTest {

        @Test
        @DisplayName("重なるミーティング")
        void overlappingMeetings() {
            MeetingTime m1 = new MeetingTime(9, 11);
            MeetingTime m2 = new MeetingTime(10, 12);
            assertThat(meetingsOverlap(m1, m2)).isTrue();
        }

        @Test
        @DisplayName("重ならないミーティング（前後）")
        void nonOverlappingMeetings() {
            MeetingTime m1 = new MeetingTime(9, 10);
            MeetingTime m2 = new MeetingTime(10, 11);
            assertThat(meetingsOverlap(m1, m2)).isFalse();
        }

        @Test
        @DisplayName("完全に含まれるミーティング")
        void containedMeeting() {
            MeetingTime m1 = new MeetingTime(9, 12);
            MeetingTime m2 = new MeetingTime(10, 11);
            assertThat(meetingsOverlap(m1, m2)).isTrue();
        }
    }

    @Nested
    @DisplayName("possibleMeetings")
    class PossibleMeetingsTest {

        @Test
        @DisplayName("既存のミーティングがない場合")
        void noExistingMeetings() {
            List<MeetingTime> possible = possibleMeetings(
                    List.empty(), 9, 12, 1);

            assertThat(possible).hasSize(3);
            assertThat(possible.map(MeetingTime::startHour)).containsExactly(9, 10, 11);
        }

        @Test
        @DisplayName("1つの既存ミーティングがある場合")
        void oneExistingMeeting() {
            List<MeetingTime> existing = List.of(new MeetingTime(10, 11));
            List<MeetingTime> possible = possibleMeetings(existing, 9, 12, 1);

            assertThat(possible).hasSize(2);
            assertThat(possible.map(MeetingTime::startHour)).containsExactly(9, 11);
        }

        @Test
        @DisplayName("複数の既存ミーティングがある場合")
        void multipleExistingMeetings() {
            List<MeetingTime> existing = List.of(
                    new MeetingTime(9, 10),
                    new MeetingTime(11, 12)
            );
            List<MeetingTime> possible = possibleMeetings(existing, 9, 13, 1);

            assertThat(possible).hasSize(2);
            assertThat(possible.map(MeetingTime::startHour)).containsExactly(10, 12);
        }

        @Test
        @DisplayName("2時間のミーティング")
        void twoHourMeeting() {
            List<MeetingTime> existing = List.of(new MeetingTime(10, 11));
            List<MeetingTime> possible = possibleMeetings(existing, 9, 14, 2);

            // 9-11は10-11と重なる、11-13はOK、12-14はOK
            assertThat(possible).hasSize(2);
            assertThat(possible.get(0)).isEqualTo(new MeetingTime(11, 13));
            assertThat(possible.get(1)).isEqualTo(new MeetingTime(12, 14));
        }

        @Test
        @DisplayName("空きがない場合")
        void noAvailableSlots() {
            List<MeetingTime> existing = List.of(
                    new MeetingTime(9, 10),
                    new MeetingTime(10, 11),
                    new MeetingTime(11, 12)
            );
            List<MeetingTime> possible = possibleMeetings(existing, 9, 12, 1);

            assertThat(possible).isEmpty();
        }
    }

    @Nested
    @DisplayName("calendarEntries")
    class CalendarEntriesTest {

        @Test
        @DisplayName("Alice のカレンダーを取得")
        void getAliceCalendar() {
            List<MeetingTime> entries = calendarEntries("Alice").unsafeRun();
            assertThat(entries).hasSize(2);
        }

        @Test
        @DisplayName("Bob のカレンダーを取得")
        void getBobCalendar() {
            List<MeetingTime> entries = calendarEntries("Bob").unsafeRun();
            assertThat(entries).hasSize(2);
        }

        @Test
        @DisplayName("未知のユーザーは空")
        void unknownUserHasEmptyCalendar() {
            List<MeetingTime> entries = calendarEntries("Unknown").unsafeRun();
            assertThat(entries).isEmpty();
        }
    }

    @Nested
    @DisplayName("scheduledMeetings (2人)")
    class ScheduledMeetingsTwoTest {

        @Test
        @DisplayName("2人の予定を結合")
        void combineTwoPeopleSchedules() {
            List<MeetingTime> meetings = scheduledMeetings("Alice", "Bob").unsafeRun();
            assertThat(meetings).hasSize(4);
        }
    }

    @Nested
    @DisplayName("scheduledMeetings (複数人)")
    class ScheduledMeetingsMultipleTest {

        @Test
        @DisplayName("複数人の予定を結合")
        void combineMultiplePeopleSchedules() {
            List<MeetingTime> meetings =
                    scheduledMeetings(List.of("Alice", "Bob", "Charlie")).unsafeRun();
            assertThat(meetings).hasSize(6);
        }
    }

    @Nested
    @DisplayName("findAvailableSlots")
    class FindAvailableSlotsTest {

        @Test
        @DisplayName("2人の空き時間を検索")
        void findSlotsForTwo() {
            List<MeetingTime> slots = findAvailableSlots(
                    List.of("Alice", "Bob"), 9, 17, 1
            ).unsafeRun();

            // Alice: 9-10, 14-15
            // Bob: 10-11, 15-16
            // 空いているのは: 11, 12, 13, 16
            assertThat(slots.map(MeetingTime::startHour)).contains(11, 12, 13, 16);
        }

        @Test
        @DisplayName("空きがない場合")
        void noSlotsAvailable() {
            // 全員の予定をカバーする時間帯
            List<MeetingTime> slots = findAvailableSlots(
                    List.of("Alice", "Bob", "Charlie"), 9, 11, 2
            ).unsafeRun();

            // 9-11は全員に予定がある
            assertThat(slots).isEmpty();
        }
    }

    @Nested
    @DisplayName("scheduleFirstAvailable")
    class ScheduleFirstAvailableTest {

        @Test
        @DisplayName("最初の空き時間でスケジュール")
        void schedulesFirstSlot() {
            Boolean scheduled = scheduleFirstAvailable(
                    List.of("Alice", "Bob"), 9, 17, 1
            ).unsafeRun();

            assertThat(scheduled).isTrue();
        }

        @Test
        @DisplayName("空きがない場合は false")
        void returnsFalseWhenNoSlots() {
            Boolean scheduled = scheduleFirstAvailable(
                    List.of("Alice", "Bob", "Charlie"), 9, 11, 2
            ).unsafeRun();

            assertThat(scheduled).isFalse();
        }
    }
}
