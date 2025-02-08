    def sort(self):
        """ Sorts the events by time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> s = str(alarm_clock)
        >>> s.find("event1") < s.find("event2")
        True
        """
