package ch08;

import io.vavr.collection.List;

/**
 * 第8章: ミーティングスケジューリングの例
 *
 * IO モナドを使った実践的な副作用の管理
 */
public final class SchedulingMeetings {

    private SchedulingMeetings() {}

    // ============================================
    // データ型
    // ============================================

    /**
     * ミーティング時間
     */
    public record MeetingTime(int startHour, int endHour) {

        public MeetingTime {
            if (startHour < 0 || startHour > 23) {
                throw new IllegalArgumentException("startHour must be 0-23");
            }
            if (endHour < 0 || endHour > 24) {
                throw new IllegalArgumentException("endHour must be 0-24");
            }
            if (startHour >= endHour) {
                throw new IllegalArgumentException("startHour must be less than endHour");
            }
        }

        public int lengthHours() {
            return endHour - startHour;
        }
    }

    // ============================================
    // 純粋関数（副作用なし）
    // ============================================

    /**
     * 2つのミーティングが重なるかどうか
     */
    public static boolean meetingsOverlap(MeetingTime m1, MeetingTime m2) {
        return m1.startHour() < m2.endHour() && m2.startHour() < m1.endHour();
    }

    /**
     * 既存のミーティングと重ならない空き時間スロットを計算
     */
    public static List<MeetingTime> possibleMeetings(
            List<MeetingTime> existingMeetings,
            int startHour,
            int endHour,
            int lengthHours) {

        return List.range(startHour, endHour - lengthHours + 1)
                .map(start -> new MeetingTime(start, start + lengthHours))
                .filter(slot -> existingMeetings.forAll(
                        existing -> !meetingsOverlap(existing, slot)));
    }

    // ============================================
    // 外部 API のシミュレーション
    // ============================================

    // 実際のアプリケーションでは外部 API を呼び出す
    // ここではシミュレーションとして固定データを返す

    private static List<MeetingTime> simulateCalendarApi(String name) {
        return switch (name.toLowerCase()) {
            case "alice" -> List.of(
                    new MeetingTime(9, 10),
                    new MeetingTime(14, 15)
            );
            case "bob" -> List.of(
                    new MeetingTime(10, 11),
                    new MeetingTime(15, 16)
            );
            case "charlie" -> List.of(
                    new MeetingTime(9, 11),
                    new MeetingTime(13, 14)
            );
            default -> List.empty();
        };
    }

    // ============================================
    // IO を使った API ラッパー
    // ============================================

    /**
     * カレンダーエントリを取得（IO でラップ）
     */
    public static IO<List<MeetingTime>> calendarEntries(String name) {
        return IO.delay(() -> simulateCalendarApi(name));
    }

    /**
     * ミーティングを作成（IO でラップ）
     */
    public static IO<Void> createMeeting(List<String> attendees, MeetingTime meeting) {
        return IO.effect(() -> {
            System.out.println("Creating meeting for " + attendees.mkString(", ") +
                    " at " + meeting.startHour() + ":00 - " + meeting.endHour() + ":00");
        });
    }

    // ============================================
    // IO を使ったスケジューリング
    // ============================================

    /**
     * 2人の予定を取得
     */
    public static IO<List<MeetingTime>> scheduledMeetings(String person1, String person2) {
        return calendarEntries(person1)
                .flatMap(p1Entries -> calendarEntries(person2)
                        .map(p2Entries -> p1Entries.appendAll(p2Entries)));
    }

    /**
     * 複数人の予定を取得
     */
    public static IO<List<MeetingTime>> scheduledMeetings(List<String> attendees) {
        return IO.traverse(attendees, SchedulingMeetings::calendarEntries)
                .map(lists -> lists.flatMap(x -> x));
    }

    /**
     * 空き時間を検索
     */
    public static IO<List<MeetingTime>> findAvailableSlots(
            List<String> attendees,
            int startHour,
            int endHour,
            int lengthHours) {

        return scheduledMeetings(attendees)
                .map(existing -> possibleMeetings(existing, startHour, endHour, lengthHours));
    }

    /**
     * ミーティングをスケジュール（最初の空きスロットを使用）
     */
    public static IO<Boolean> scheduleFirstAvailable(
            List<String> attendees,
            int startHour,
            int endHour,
            int lengthHours) {

        return findAvailableSlots(attendees, startHour, endHour, lengthHours)
                .flatMap(slots -> {
                    if (slots.isEmpty()) {
                        return IO.pure(false);
                    } else {
                        return createMeeting(attendees, slots.head())
                                .andThen(IO.pure(true));
                    }
                });
    }

    // ============================================
    // リトライ付きスケジューリング
    // ============================================

    /**
     * カレンダー取得をリトライ付きで実行
     */
    public static IO<List<MeetingTime>> calendarEntriesWithRetry(String name, int maxRetries) {
        return calendarEntries(name)
                .retry(maxRetries)
                .orElse(List.empty());
    }

    /**
     * 複数人の予定をリトライ付きで取得
     */
    public static IO<List<MeetingTime>> scheduledMeetingsWithRetry(
            List<String> attendees,
            int maxRetries) {

        return IO.traverse(attendees, name -> calendarEntriesWithRetry(name, maxRetries))
                .map(lists -> lists.flatMap(x -> x));
    }
}
