from gtts import gTTS
import pygame


def text_to_speech(text):
    tts = gTTS(text=text, lang='en')
    filename = "speech.mp3"
    tts.save(filename)
    pygame.mixer.init()
    pygame.mixer.music.load(filename)
    pygame.mixer.music.play()
    while pygame.mixer.music.get_busy():
        pass

def main():
    text_to_speech("Hello, this is a speech synthesis test.")


if __name__ == "__main__":
    main()
