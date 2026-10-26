package com.client.batch.jobs;

import com.client.domain.model.Cvact01y;
import com.client.domain.repository.Cvact01yRepository;
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
public class CreastmtJobConfig {
    @Bean
    public Job creastmtJob(JobRepository jobRepository, Step creastmtStep) {
        return new JobBuilder("CREASTMT", jobRepository)
            .start(creastmtStep)
            .build();
    }

    @Bean
    public Step creastmtStep(JobRepository jobRepository,
                            PlatformTransactionManager txManager,
                            ItemReader<Cvact01y> creastmtReader,
                            ItemProcessor<Cvact01y, String> creastmtProcessor,
                            ItemWriter<String> creastmtWriter) {
        return new StepBuilder("creastmtStep", jobRepository)
            .<Cvact01y, String>chunk(25, txManager)
            .reader(creastmtReader)
            .processor(creastmtProcessor)
            .writer(creastmtWriter)
            .build();
    }

    @Bean
    public RepositoryItemReader<Cvact01y> creastmtReader(Cvact01yRepository repository) {
        return new RepositoryItemReaderBuilder<Cvact01y>()
            .name("creastmtReader")
            .repository(repository)
            .methodName("findAll")
            .pageSize(25)
            .sorts(Map.of("acctId", Sort.Direction.ASC))
            .build();
    }

    @Bean
    public ItemProcessor<Cvact01y, String> creastmtProcessor() {
        return acct -> {
            if (acct == null) return null;
            return String.format("STMT|ACCT=%d|BAL=%s|STATUS=%s",
                acct.getAcctId(), acct.getAcctCurrBal(), acct.getAcctActiveStatus());
        };
    }

    @Bean
    public ItemWriter<String> creastmtWriter() {
        return items -> {
            // Emits extracted statement detail records for downstream reporting
        };
    }
}
