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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;

@Configuration
public class IntcalcJobConfig {
    @Bean
    public Job intcalcJob(JobRepository jobRepository, Step intcalcStep) {
        return new JobBuilder("INTCALC", jobRepository)
            .start(intcalcStep)
            .build();
    }

    @Bean
    public Step intcalcStep(JobRepository jobRepository,
                           PlatformTransactionManager txManager,
                           ItemReader<Cvact01y> intcalcReader,
                           ItemProcessor<Cvact01y, Cvact01y> intcalcProcessor,
                           ItemWriter<Cvact01y> intcalcWriter) {
        return new StepBuilder("intcalcStep", jobRepository)
            .<Cvact01y, Cvact01y>chunk(25, txManager)
            .reader(intcalcReader)
            .processor(intcalcProcessor)
            .writer(intcalcWriter)
            .build();
    }

    @Bean
    public RepositoryItemReader<Cvact01y> intcalcReader(Cvact01yRepository repository) {
        return new RepositoryItemReaderBuilder<Cvact01y>()
            .name("intcalcReader")
            .repository(repository)
            .methodName("findAll")
            .pageSize(25)
            .sorts(Map.of("acctId", Sort.Direction.ASC))
            .build();
    }

    @Bean
    public ItemProcessor<Cvact01y, Cvact01y> intcalcProcessor() {
        return acct -> {
            if (acct == null || !"Y".equalsIgnoreCase(acct.getAcctActiveStatus())) {
                return null;
            }
            BigDecimal bal = acct.getAcctCurrBal() != null ? acct.getAcctCurrBal() : BigDecimal.ZERO;
            if (bal.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal rate = new BigDecimal("12.00");
                BigDecimal interest = bal.multiply(rate).divide(new BigDecimal("1200"), 2, RoundingMode.HALF_UP);
                acct.setAcctCurrBal(bal.add(interest));
            }
            return acct;
        };
    }

    @Bean
    public ItemWriter<Cvact01y> intcalcWriter(Cvact01yRepository repository) {
        return repository::saveAll;
    }
}
