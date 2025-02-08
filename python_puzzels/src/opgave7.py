class AlarmClock:
    """ Represents an alarm clock that can handle events. """

    def __init__(self):
        """ Initialises an AlarmClock object with an empty list of events.
        >>> alarm_clock = AlarmClock()
        """

    def add_event(self, event):
        """ Adds an 'event' to this AlarmClock object, it doesn't return anything.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        """

    def __repr__(self):
        """ Returns a string representation of the AlarmClock object.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        >>> s = str(alarm_clock)
        >>> "18:30:00" in s
        True
        >>> "dinner" in s
        True
        >>> "breakfast" in s
        False
        """
