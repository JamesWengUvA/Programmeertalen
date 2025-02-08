    def get_next_event(self):
        """ Returns the next event with the smallest time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.get_next_event().get_description()
        'event2'
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> alarm_clock.get_next_event().get_description()
        'event1'
        """

    def remove_next_event(self):
        """ Removes and returns the next event with the smallest time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> alarm_clock.remove_next_event().get_description()
        'event1'
        >>> alarm_clock.remove_next_event().get_description()
        'event2'
        """
