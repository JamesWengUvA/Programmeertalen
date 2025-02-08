class Event:
    """ Represents an event that happens at a certain time."""

    def __init__(self, time, description):
        """ Initialises an Event object with a 'time' object of type Time and a
        'description' of type str.
        >>> event = Event(Time(18, 30, 0), "dinner")
        """

    def __repr__(self):
        """ Returns the string representation of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner") )
        18:30:00 dinner
        """

    def get_time(self):
        """ Returns the time of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_time() )
        18:30:00
        """

    def get_description(self):
        """ Returns the description of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_description() )
        dinner
        """
