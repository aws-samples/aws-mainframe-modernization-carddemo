package com.carddemo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class CarddemoApplication {
    public static void main(String[] args) {
        SpringApplication.run(CarddemoApplication.class, args);
    }
}
