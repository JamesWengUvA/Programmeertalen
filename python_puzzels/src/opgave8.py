    def __len__(self):
        """ Returns the number of events in this AlarmClock object.
        >>> alarm_clock = AlarmClock()
        >>> len(alarm_clock)
        0
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        >>> len(alarm_clock)
        1
        """
