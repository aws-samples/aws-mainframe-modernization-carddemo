package com.carddemo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import java.time.Clock;

@SpringBootApplication
public class CardDemoApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoApplication.class, args);
    }

    /** Injectable clock so date edits (e.g. DOB-in-the-past) are testable. */
    @Bean
    public Clock clock() {
        return Clock.systemDefaultZone();
    }
}
