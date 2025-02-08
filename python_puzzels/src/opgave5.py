import time

# <here goes the Time class with its methods>


def get_current_hours_minutes_seconds():
    """ Returns the current (hours, minutes, seconds) as a tuple. """
    t = time.localtime()
    return (t.tm_hour, t.tm_min, t.tm_sec)


def now():
    """ Returns the current time as Time object. """
    return Time(*get_current_hours_minutes_seconds())
