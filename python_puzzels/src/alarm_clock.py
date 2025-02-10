import time
import memory_graph as mg
from gtts import gTTS
import pygame

HOURS_IN_DAY = 24
MINUTES_IN_HOUR = 60
SECONDS_IN_MINUTE = 60


class Time:
    """ Represents a time of day."""

    def __init__(self, hours, minutes, seconds):
        """ Initialises a Time object with integers 'hours', 'minutes' and
        'seconds.
        >>> t = Time(18, 30, 0)
        """
        self.set_time(hours, minutes, seconds)

    def __repr__(self):
        """ Returns the string representation of a Time object.
        >>> print( Time(8,5,30) )
        08:05:30
        """
        return f"{self.hours:02}:{self.minutes:02}:{self.seconds:02}"

    def __add__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' added to 'self'.
        >>> print(Time(0,0,0) + Time(1,2,3))
        01:02:03
        >>> print(Time(13,30,0) + Time(1,46,-45))
        15:15:15
        """
        return Time(self.hours + other.hours, self.minutes + other.minutes,
                    self.seconds + other.seconds)

    def __sub__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' substracted from 'self'.
        >>> print(Time(10,10,10) - Time(1,2,3))
        09:08:07
        >>> print(Time(10,0,0) - Time(1,50,600))
        08:00:00
        """
        return Time(self.hours - other.hours, self.minutes - other.minutes,
                    self.seconds - other.seconds)

    def set_time(self, hours, minutes, seconds):
        """ Sets the time of the Time object to 'hours', 'minutes',
        and 'seconds' making sure the values are in valid range:
          hours:   [0, HOURS_IN_DAY)
          minutes: [0, MINUTES_IN_HOUR)
          seconds: [0, SECONDS_IN_MINUTE)
        >>> time = Time(0, 0, 0)
        >>> time.set_time(0, 0, 90)
        >>> print(time)
        00:01:30
        >>> time.set_time(0, 0, 3600)
        >>> print(time)
        01:00:00
        >>> time.set_time(0, 0, -1)
        >>> print(time)
        23:59:59
        >>> time.set_time(10, -121, 0)
        >>> print(time)
        07:59:00
        >>> time.set_time(-50, 0, 0)
        >>> print(time)
        22:00:00
        >>> print(Time(10, -120, -150)) # __init__() test
        07:57:30
        """
        extra_minutes, self.seconds = divmod(seconds, SECONDS_IN_MINUTE)
        extra_hours, self.minutes = divmod(minutes + extra_minutes,
                                           MINUTES_IN_HOUR)
        self.hours = (hours + extra_hours) % HOURS_IN_DAY

    def get_hours(self):
        """ Returns the hours of the Time object.
        >>> Time(23,0,0).get_hours()
        23
        """
        return self.hours

    def get_minutes(self):
        """ Returns the minutes of the Time object.
        >>> Time(0,59,0).get_minutes()
        59
        """
        return self.minutes

    def get_seconds(self):
        """ Returns the seconds of the Time object.
        >>> Time(0,0,59).get_seconds()
        59
        """
        return self.seconds

    def get_total_seconds(self):
        """ Returns the number of seconds since time 00:00:00.
        >>> Time(0,0,1).get_total_seconds()
        1
        >>> Time(0,1,0).get_total_seconds()
        60
        >>> Time(1,0,0).get_total_seconds()
        3600
        >>> Time(13,30,5).get_total_seconds()
        48605
        """
        return ((self.hours * MINUTES_IN_HOUR + self.minutes)
                * SECONDS_IN_MINUTE + self.seconds)


class Event:
    """ Represents an event that happens at a certain time."""

    def __init__(self, time, description):
        """ Initialises an Event object with a 'time' object of type Time and a
        'description' of type str.
        >>> event = Event(Time(18, 30, 0), "dinner")
        """
        self.time = time
        self.description = description

    def __repr__(self):
        """ Returns the string representation of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner") )
        18:30:00 dinner
        """
        return f"{self.time} {self.description}"

    def get_time(self):
        """ Returns the time of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_time() )
        18:30:00
        """
        return self.time

    def get_description(self):
        """ Returns the description of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_description() )
        dinner
        """
        return self.description


class AlarmClock:
    """ Represents an alarm clock that can handle events. """

    def __init__(self):
        """ Initialises an AlarmClock object with an empty list of events.
        >>> alarm_clock = AlarmClock()
        """
        self.events = list()

    def add_event(self, event):
        """ Adds an 'event' to this AlarmClock object, it doesn't return anything.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        """
        self.events.append(event)
        self.sort()

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
        return f"{self.events}"

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
        return len(self.events)

    def sort(self):
        """ Sorts the events by time.
        >>> alarm_clock = AlarmClock()
        >>> alarm_clock.add_event( Event(Time(0, 0, 2), "event2") )
        >>> alarm_clock.add_event( Event(Time(0, 0, 1), "event1") )
        >>> s = str(alarm_clock)
        >>> s.find("event1") < s.find("event2")
        True
        """
        self.events.sort(key=lambda event : str(event))

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
        return self.events[0]

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
        return self.events.pop(0)

    def wait_for_and_handle_events(self):
        """ Wait for each event to pass and then print the event. """
        while self:
            current_time = now()
            event = self.remove_next_event()
            time.sleep((event.get_time() - current_time).get_total_seconds())
            print("ALARM:", event)
            text_to_speech(event.get_description())


def text_to_speech(text):
    tts = gTTS(text=text, lang='en')
    filename = "speech.mp3"
    tts.save(filename)
    pygame.mixer.init()
    pygame.mixer.music.load(filename)
    pygame.mixer.music.play()
    while pygame.mixer.music.get_busy():
        pass


def get_current_hours_minutes_seconds():
    """ Returns the current (hours, minutes, seconds) as a tuple. """
    t = time.localtime()
    return (t.tm_hour, t.tm_min, t.tm_sec)


def now():
    """ Returns the current time as Time object. """
    return Time(*get_current_hours_minutes_seconds())


def main():
    alarm_clock = AlarmClock()
    alarm_clock.add_event(Event(now() + Time(0, 0, 1), "eat some breakfast"))
    alarm_clock.add_event(Event(now() + Time(0, 0, 6), "off to work"))
    alarm_clock.add_event(Event(now() + Time(0, 0, 11), "good morning, wake up"))
    alarm_clock.wait_for_and_handle_events()


if __name__ == "__main__":
    main()
