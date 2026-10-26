package com.client.batch.jobs;

import com.client.domain.model.Cvtra01y;
import com.client.domain.repository.Cvtra01yRepository;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.item.data.RepositoryItemReader;
import org.springframework.batch.item.data.builder.RepositoryItemReaderBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Sort;
import org.springframework.transaction.PlatformTransactionManager;

import java.util.Map;

@Configuration
public class PosttranJobConfig {
    @Bean
    public Job posttranJob(JobRepository jobRepository, Step posttranStep) {
        return new JobBuilder("POSTTRAN", jobRepository)
            .start(posttranStep)
            .build();
    }

    @Bean
    public Step posttranStep(JobRepository jobRepository,
                            PlatformTransactionManager txManager,
                            ItemReader<Cvtra01y> posttranReader,
                            ItemProcessor<Cvtra01y, Cvtra01y> posttranProcessor,
                            ItemWriter<Cvtra01y> posttranWriter) {
        return new StepBuilder("posttranStep", jobRepository)
            .<Cvtra01y, Cvtra01y>chunk(25, txManager)
            .reader(posttranReader)
            .processor(posttranProcessor)
            .writer(posttranWriter)
            .build();
    }

    @Bean
    public RepositoryItemReader<Cvtra01y> posttranReader(Cvtra01yRepository repository) {
        return new RepositoryItemReaderBuilder<Cvtra01y>()
            .name("posttranReader")
            .repository(repository)
            .methodName("findAll")
            .pageSize(25)
            .sorts(Map.of("tranId", Sort.Direction.ASC))
            .build();
    }

    @Bean
    public ItemProcessor<Cvtra01y, Cvtra01y> posttranProcessor() {
        return item -> {
            if (item == null) return null;
            item.setTranDesc("POSTED-" + (item.getTranDesc() != null ? item.getTranDesc() : "DAILY"));
            return item;
        };
    }

    @Bean
    public ItemWriter<Cvtra01y> posttranWriter(Cvtra01yRepository repository) {
        return repository::saveAll;
    }
}
